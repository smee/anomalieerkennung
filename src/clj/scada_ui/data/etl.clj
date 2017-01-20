(ns ^{:doc "ETL pipeline for GlobaltechI daily 10min CSV files"}
    scada-ui.data.etl
  (:require [clojure.edn :as edn]
            [clj-time
             [coerce :as tc]
             [core :as time]
             [format :as tf]
             [periodic :as tp]]
            [clojure.java.jdbc :as sql] 
            [clojure
             [string :as str]
             [zip :as zip]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [scada-ui.data
             [csv :as csv]
             [db :as db]]
            [scada-ui.util.tree :as tree]
            [org.clojars.smee
             [file :refer [find-files with-temp-file relative-path]]]
            [streaming.protocols :as sp]
            [timeseries.features :as feat]
            [scada-ui.data.structure :as cache]
            [scada-ui.data.structure :as struc])
  (:import java.io.File
           [java.text Format SimpleDateFormat]
           [java.util Calendar Date Locale TimeZone]))

(defn error-id [component event]
  (str "error_" component "_" event))

(defn parameter-id [type component parameter]
  (str/join "_" ["parameter" type component parameter]))

(defn init-db!
"Make sure we have all database tables we need as well as imported plants and plant specs. Idempotent."
  [conn plants plant-specs]
  (db/create-tables! conn)
  (let [known-spec? (db/plant-spec-names conn)]
    (doseq [{n :name :as spec} plant-specs :when (not (known-spec? n))]
      (println "inserting plant spec" (:name spec))
      (db/insert-plant-types! conn [spec])))
  (doseq [{:keys [name type] :as plant} plants]
    (let [{{cols :data trace-names :traces} :columns :as plant-spec} (some #(when (= type (:name %)) %) plant-specs)
          col-names (map (fn [args] (str/join "/" (map clojure.core/name args))) cols)
          cmd (db/create-series-table-command name col-names)]
      (db/do! conn cmd)
      (when (not-empty trace-names)        
        (db/do! conn (db/create-series-table-command (str name "-traces") trace-names)))
      (when (not (db/plant conn name))
        (db/insert! conn :plants [plant])))))

(defn read-edn [file]
  (with-open [is (io/reader file)] (edn/read (java.io.PushbackReader. is))))

(defn initialize-database!
"initialize db with plant specific tables, plant series metadata. Expects to find two files in `input-dir`: 'M5000-de.xml' with the plant's structure, 'coordinates.edn' with the plant coordinates.
This function is idempotent, can be run arbitrarily."
  [conn db-name input-dir]
  (let [plant-specs (read-edn (io/file input-dir "plant-specs.edn"))
        plants (read-edn (io/file input-dir "plants.edn"))]
    (println "initializing database metadata, tables, ...")
    (init-db! conn plants plant-specs)
    (println "done intializing database structures!")))


(def dateformat-date (tf/formatter "yyyy-MM-dd"))
(def trace-dateformat (tf/formatter "yyyy-MM-dd-HH'h'mm'm'ss's'"))


(def ^{:doc "Date format in DataOpMode files"} events-date-format (tf/formatter time/utc "yyyy-MM-dd-HH:mm:ss"  "yyyy-MM-dd-HH:mm:ss,SSS"))

(defn reconstruct-events
"Given the events from DataOpMode files, reconstruct intervals of operation status times and errors.
Returns a vector in the same order as expected by the database table."
  [raw-events]
  (let [dt (fn [^String date] (tf/parse events-date-format date))]
    (loop [[[date user old-mode new-mode _ component error-id :as event] & events] raw-events,
           res [],
           mode nil
           error nil]
      (if (not event)
        (if-let [last-date (or (:start mode) (:start error))]
          ;;there is a mode or error interval in progress, use midnight of the same day as the end
          (let [midnight (time/plus (time/with-time-at-start-of-day last-date) (time/days 1))]
            (cond-> res
              mode (conj (assoc mode :end midnight))
              error (conj (assoc error :end midnight))))
          res)
        (let [date (dt date)
              ;; we have a change if either this is the first event of the day or there is a switch to a new mode we are not in currently (there may be multiple switches within one second to the same new mode)
              mode-changed? (or (nil? mode)
                                (and (not= old-mode new-mode) mode (not= new-mode (:id mode))))
              mode' (if mode-changed?
                      ;; if this is the first event, we know that the status must have been already valid at midnight,
                      ;; the SCADA system tends to log the first event some milliseconds past midnight
                      {:start (if (and (empty? res) (nil? mode)) (time/with-time-at-start-of-day date) date) :user user :type "status" :id new-mode}
                      mode)
              id (error-id component error-id)
              error-started? (and (not= "0" error-id) ;; we have a current error
                                  (not= id (:id error)) ;; the error id is different
                                  )
              error-completed? (and error (or (= "0" error-id) (not= id (:id error))))            
              error' (cond
                       error-started? {:start date :user user :type "error" :id id}
                       error-completed? nil
                       :else error)]
          (recur events
                 (cond-> res
                   error-completed? (conj (assoc error :end date))
                   (and mode mode-changed?) (conj (assoc mode :end date)))
                 mode'
                 error'))))))

(defn create-events [input-file]
  (with-open [rdr (-> input-file io/reader)]
    (->> rdr
         csv/parse-csv
         (map (partial take 7))
         ;; the first few event files per power plant have too little columns
         ;; the last three columns are irrelevant
         (filter #(= 7 (count %)))
         distinct
         reconstruct-events)))

(defn create-event-csv
  [plant events output-file]
  (csv/write-csv output-file
                 (for [{:keys [end start user type id]} events]
                   (into (db/create-datetime-entries start) (vector (tf/unparse db/dateformat-full end) (tc/to-long end) plant user type id)))))


(defn create-production-column
  "Each plant might be either fully in production during each measurement time interval
  or there is some overlap with non-production durations. Returns a column for the whole time
  interval of `events` with a fixed timestep length that contains `1` if the power plant
  was in production during the full timestep, else `0`."
  [events {sr-in-s :sampling-rate}]
  (let [start (:start (first events))
        end (:end (last events))
        production? #{"7"}
        without-seconds (fn [ts] ;; can't use time/floor, does not exist in cljs-time
                          (time/date-time (time/year ts) (time/month ts) (time/day ts) (time/hour ts) (time/minute ts)))
        intervals (->> events
                       (filter #(production? (str (:id %))))
                       (keep (fn [{e :end ts :start}]
                               ;; make sure we remove partial time intervals
                               ;; we are only interested in full 10min interval measurements.
                               ;; so if any interval is overlapping the 10min boundaqry we skip forward/backward to the next/last 10min boundary
                               (let [s-minutes (mod (+ (time/second ts) (* 60 (time/minute ts))) sr-in-s)
                                     e-minutes (mod (+ (time/second e) (* 60 (time/minute e))) sr-in-s)
                                     ts (without-seconds (if (zero? s-minutes) ts (time/plus ts (time/seconds (- sr-in-s s-minutes)))))
                                     e (without-seconds(if (zero? e-minutes) e (time/minus e (time/seconds e-minutes))))]
                                 (when (time/before? ts e)
                                   (time/interval ts e))))))]
       (loop [res [], [ts & timestamps] (tp/periodic-seq start end (time/seconds sr-in-s))]
         (if (not ts)
           res
           (if (some #(time/within? % ts) intervals)
             (recur (conj res 1.0) timestamps)
             (recur (conj res 0.0) timestamps))))))

(defn create-max-power-columns
"We have manually set power restrictions as parameter change events. We need to represent them
as sensor data. To do so, we need to calculate min, max, and weighted average and standard deviation
per sampling time interval. We need access to the database to determine the last known good value,
because the switch events may either not contain a relevant entry or there were some missing files."
  [last-value plant changes start end {:keys [sampling-rate max-power]}]
  (let [a ["_reduction" :avg] 
        mi ["_reduction" :min]
        ma ["_reduction" :max]
        s ["_reduction" :std]
        max-power (struc/max-power plant)
        crnt (or last-value {a max-power mi max-power ma max-power s 0.0})
        timestamps (tp/periodic-seq start end (time/seconds sampling-rate))
        n (count timestamps)
        switches (filter #(= "parameter_2_15_32" (:id %)) changes)
        add-n (fn [res n m]
                (reduce-kv (fn [res k v]
                             (update res k into (repeat n v))) res m))]
    (if (empty? switches)
      (with-meta {a (repeat n (crnt a))
                  mi (repeat n (crnt mi))
                  ma (repeat n (crnt ma))
                  s (repeat n (crnt s))}
        {:last-reduction crnt})
      (loop [res {a []
                  mi []
                  ma []
                  s []}
             crnt crnt
             timestamps timestamps
             switches switches]
        (if (empty? switches)
          (with-meta (add-n res (count timestamps) crnt) {:last-reduction crnt})
          ;; switch happens within a time interval, calculate correct average, standard deviation
          (let [sw-ts (:timestamp (first switches))
                [before after] (split-with #(time/before? % sw-ts) timestamps)
                [before [last-before]] (split-at (dec (count before)) before)
                last-before (or last-before (first after)) ;; if the switch occurred exactly on a time interval boundary, there may be no timestamps before
                res (add-n res (count before) crnt)
                end (time/plus last-before (time/seconds sampling-rate))
                switches-in-period (take-while #(not (time/after? (:timestamp %) end)) switches)
                ;; at what percentages of a sampling interval did the changes occur?
                weights (->> (concat [last-before]
                                     (map :timestamp switches-in-period)
                                     [end])
                             (partition 2 1)
                             (map (fn [[a b]] (/ (time/in-seconds (time/interval a b)) (double sampling-rate)))))
                new-vs (map :new switches-in-period)
                new (:new (last switches-in-period))
                w-avg (reduce + (map * weights (cons (crnt a) new-vs)))
                w-sq (reduce + (map (fn [w v] (* w v v)) weights (cons (crnt a) new-vs)))]
            (recur (add-n res 1 {mi (reduce min (cons (crnt mi) new-vs))
                                 ma (reduce max (cons (crnt ma) new-vs))
                                 a w-avg
                                 s (Math/sqrt (- w-sq w-avg))})                        
                   {mi new ma new a new s 0.0}
                   after
                   (nthnext switches (count switches-in-period)))))))))

(defn create-infobright-csv
  "Converts all csv files in a directory to csv files importable into infobright according
to the table spec in db.cljs
Needs the explicit order of column names. Will fill in missing columns with zeros."
  [{{column-order :data} :columns {prod-sensor :production-sensor} :status :as plant-spec}
   production-column power-columns input-file out-file]
  (let [input-file (io/as-file input-file)
        out-file (io/as-file out-file)]
    (let [filename (.getName input-file)
          date-s (subs filename 0 (.indexOf filename ".txt"))
          date (tf/parse dateformat-date date-s)
          in (io/input-stream input-file)
          ;; skip header, always constant
          columns (csv/parse-10mwk-file in date)]
      (when (not-empty columns)
        ;; only run this if there is at least one valid column
        ;; else we will try to create unlimited rows because
        ;; we have no reference for column length
        (let [sorted-columns (for [cn (cons "timestamp" column-order)
                                  :let [col (get columns cn ::dummy)]]
                               (cond
                                 (= cn [prod-sensor :avg]) production-column
                                 (power-columns cn) (power-columns cn)
                                 (= col ::dummy) (do ;(println "Warning: missing column" cn)
                                                   (repeat 0))
                                 :else col))
              rows (apply map vector sorted-columns)]
          (csv/write-csv out-file
                         (for [[date & vs] rows]
                           (into (db/create-datetime-entries date) vs)))))))
    out-file)

(defn parameter-changes [input-file]
  (if (not (.exists (io/as-file input-file)))
    []
    (let [input-df (tf/formatter time/utc "yyyy-MM-dd-HH:mm:ss"  "yyyy-MM-dd-HH:mm:ss,SSS")
          df (java.text.NumberFormat/getNumberInstance)
          dbl #(.parse df ^String %)]
      (with-open [rdr (-> input-file io/reader)]
        (->> rdr
             csv/parse-csv
             (mapv (fn [[date user _ type component channel new old]]
                     (let [id (parameter-id type component channel)
                           old (case old
                                 "TRUE" 1.0
                                 "FALSE" 0.0
                                 (dbl old))
                           new (case new
                                 "TRUE" 1.0
                                 "FALSE" 0.0
                                 (dbl new))]
                       {:timestamp (tf/parse input-df date)
                        :user user
                        :id id
                        :new new
                        :old old}))))))))


(defn create-parachange-csv
  [plant changes output-file]
  (csv/write-csv output-file
    (for [{:keys [timestamp user id new old]} changes]
       (into (db/create-datetime-entries timestamp) [plant user id new old]))))

(let [nf (java.text.NumberFormat/getNumberInstance Locale/US)]
  (defn s->num [^String s]
    (.parse nf s))
  (defn num->s [^Number n]
    (.format nf n)))

(defn insert-decimal
"Trace data entries are always integers with a divisor. To avoid expensive parsing and formatting
of floats this function just inserts a decimal point to convert a integer string to a scaled
floating point number. Works only for divisor in 10^x for x in [1,2,3..]"
  [^String s ^String divisor]
  (let [negative? (= \- (.charAt s 0))
        s (if negative? (subs s 1) s)
        n (dec (.length divisor))
        |s| (.length s)
        s (if (>= n |s|)
            (str (apply str (repeat (- n |s| -1) "0")) s)
            s)
        |s| (.length s)
        cut-idx (- |s| n)
        pre (subs s 0 cut-idx)
        post (subs s cut-idx)]
    (str (when negative? "-") pre "." post)))

(defn create-trace-csv
  "Create importable CSV file for infobright. `col-fn` takes two string arguments: `component id`,`trace sensor id`"
  [{{trace-columns :traces} :columns :as plant-spec} input-file output-file]
  (let [file (io/as-file input-file)
        n (.getName file)
        dir (.getParent file)
        [_ prefix date-str ms c err] (re-find #"M5000_(\d+)_(.*)_(\d\d\d\d)ms_KompNo-(\d+)_ErrNo-(\d+).trc.gz" n)
        timestamp (tf/parse trace-dateformat date-str)        
        columns (->> (for [interval [10 200 1000]
                           :let [file (io/file dir (format "M5000_%s_%s_%04dms_KompNo-%s_ErrNo-%s.trc.gz" prefix date-str interval c err))]
                           :when (.exists file)
                           :let [timestamps (tp/periodic-seq (time/minus timestamp (time/millis (* 300 interval)))
                                                             (time/plus timestamp (time/millis (* 100 interval)))
                                                             (time/millis interval))]]                       
                       (with-open [rdr (io/reader (java.util.zip.GZIPInputStream. (io/input-stream file)))]
                         (let [[[_ n] & rows] (csv/parse-csv rdr)
                               event-idx (s->num n)]
                           (into {:timestamps timestamps}
                                 (for [[type component id divisor & vs] rows
                                       :when (= type "aK")
                                       :let [series-name (str component "/" id)]
                                       :let [;; rescale values, keep as strings if possible
                                             vs (cond
                                                  (= "1" divisor) vs
                                                  (#{"10" "100" "1000" "10000"} divisor) (map #(insert-decimal % divisor) vs)
                                                  :else (let [d (s->num divisor)] (map #(/ (s->num %) d) vs)))
                                             vs (vec (take 400 (drop event-idx (cycle vs))))]]
                                   [series-name vs])))))
                     ;; combine different time resolutions: replace with increasingly finer resolutions
                     (reduce (fn [{ts1 :timestamps :as m1} {ts2 :timestamps :as m2}]
                               (let [m-lowres (if (time/before? (first ts1) (first ts2)) m1 m2)
                                     m-highres (if (time/before? (first ts1) (first ts2)) m2 m1)
                                     t-lowres (:timestamps m-lowres)
                                     t-highres (:timestamps m-highres)
                                     thr-fst (first t-highres)
                                     thr-lst (last t-highres)
                                     [before in] (split-with #(time/before? % thr-fst) t-lowres)
                                     n1 (count before)
                                     [in after] (split-with #(time/before? % thr-lst) in)
                                     n2 (count in)]
                                 (into {} (map (fn [[k vs]]
                                                 (let [other (get m-highres k)
                                                       [before rest] (split-at n1 vs)]
                                                   [k (vec (concat before other (drop n2 rest)))]))
                                               m-lowres))))))
        sorted-columns (for [cn (cons :timestamps trace-columns)
                             :let [col (get columns cn ::dummy)]]
                         (if (= col ::dummy)
                           (do ;(println "Warning: missing column" cn)
                             (repeat 0))
                           col))
        rows (apply map vector sorted-columns)]
    (csv/write-csv output-file
                   (for [[date & vs] rows]
                     (into (db/create-datetime-entries date) vs)))))


;;;;;;;;;;;;;;;; main ETL logic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^java.io.File find-matching-events-file [data-file]
  (let [data-file (io/as-file data-file)
        filename (.getName data-file)]
    (-> data-file (.getParentFile) (.getParentFile) (io/file "DataOpMode" filename))))

(defn ^java.io.File find-matching-changes-file [data-file]
  (let [data-file (io/as-file data-file)
        filename (.getName data-file)]
    (-> data-file (.getParentFile) (.getParentFile) (io/file "DataParaChange" filename))))


(def file-pattern #".*(GT\d\d).*(Data10MinMW|DataOpMode|DataParaChange|DataTrace).*\.(txt|gz)")

(defn file-metadata [^File file]
  (let [fname (.getName file)
        [_ plant type] (re-find file-pattern (str file))]
    (when (and plant type (or (not= type "DataTrace") (.contains fname "1000ms")))
      {:plant plant
       :file file
       :date (if (.endsWith fname ".txt")
               (tf/parse dateformat-date (subs fname 0 (.indexOf fname ".")))
               (tf/parse trace-dateformat (second (re-find #"M5000_\d+_([^_]+)_" fname))))
       :type type
       :table (get {"Data10MinMW" plant
                    "DataOpMode" "events"
                    "DataParaChange" "parameter-changes"
                    "DataTrace" (str plant "-traces")} type)})))

(defn find-importable-files
"Find all text files we can handle and are not yet imported in the database."
  [conn input-dir]
  (let [newest-imported-file (reduce (fn [res m] (assoc res [(:table m) (:plant m)] (:filename m))) {}
                                     (db/adhoc conn "select `table`,plant,max(filename) as filename from imported group by plant,`table`"))]
    (->> (for [file (filter #(re-matches file-pattern (str %)) (file-seq (io/as-file input-dir)))
               :let [{table :table plant :plant :as res} (file-metadata file)
                     relative (relative-path input-dir file)]
               :when (and res (pos? (compare (str relative) (get newest-imported-file [table plant]))))]
           (assoc res :relative-file relative))
         (group-by (juxt :plant (comp time/with-time-at-start-of-day :date)))
         (keep (fn [[date metas]] ;; we can't import 10min data without operation state informations
                 (if (some #(and (= "Data10MinMW" (:type %))
                                 (not (.exists (find-matching-events-file (:file %))))) metas)
                     (remove #(= "Data10MinMW" (:type %)) metas)
                     metas)))
         (reduce into []))))


(defmulti convert (fn [{:keys [type]} {:keys [csv-file] :as context}] type))

(defmethod convert "Data10MinMW" [{:keys [plant-spec file plant date]} {:keys [csv-file] :as context}]
  (let [events-file (find-matching-events-file file)
        changes-file (find-matching-changes-file file)
        reductions (create-max-power-columns (:last-reduction context) plant (parameter-changes changes-file) date (time/plus date (time/days 1)) plant-spec)] ;; this means we parse the events files twice, but it is cheap anyway
    (when (.exists events-file)
      (create-infobright-csv plant-spec
                             (create-production-column (create-events events-file) plant-spec)
                             reductions
                             file
                             csv-file))
    (assoc (or context {}) :last-reduction (-> reductions meta :last-reduction))))

(defmethod convert "DataOpMode" [{:keys [plant file]} {:keys [csv-file] :as context}]
  (create-event-csv plant (create-events file) csv-file)
  context)

(defmethod convert "DataParaChange" [{:keys [file plant]} {:keys [csv-file] :as context}]
  (create-parachange-csv plant (parameter-changes file) csv-file)
  context)

(defmethod convert "DataTrace" [{:keys [file plant-spec]} {:keys [csv-file] :as context}]
  (create-trace-csv plant-spec file csv-file)
  context)

(defn last-power-reduction
"Specific for GT1: Converted power reduction values, came from parameter-change files."
  [conn plant]
  (let [max-power (struc/max-power plant)]
    (sql/query conn [(format "select `_reduction/avg` as 'avg', `_reduction/min` as 'min', `_reduction/max` as 'max', `_reduction/std` as 'std' from `%s` order by timestamp DESC limit 1" plant)]
               {:result-set-fn (fn [[{:keys [min max avg std]}]]
                                 {["_reduction" :avg] (or avg max-power)
                                  ["_reduction" :min] (or min max-power)
                                  ["_reduction" :max] (or max max-power)
                                  ["_reduction" :std] (or std 0.0)})})))

(defn import! [conn file-maps import-dir]
  (let [grouped (->> file-maps
                     (map #(assoc % :plant-spec (cache/plant-spec-of (:plant %))))
                     (group-by (juxt :type :plant))
                     (into (sorted-map)))]
    (doseq [[[type plant] maps] grouped
            :let [maps (sort-by (comp str :file) maps)
                  context {:conn conn
                           :last-reduction (last-power-reduction conn plant)
                           :start (:date (first maps))}]]
      (loop [{:keys [csv-file start] :as context} context,
             done []
             [m & maps] maps]
        (when m
          (let [fd (tf/unparse dateformat-date (:start context))
                ld (tf/unparse dateformat-date (:date m))
                csv-file (or csv-file (io/file import-dir (format "%s-%s-%s-%s.csv" plant type fd (tree/random-id))))
                context' (convert m (assoc context :csv-file csv-file))
                file-len (.length csv-file)
                imported? (or (empty? maps) (>= file-len (* 100 1000 1000))) ;; for efficiency reasons only import csv if we either read all available files or the file to be imported is larger than 100MB                
                ]                 
            (when imported?
              (println plant type file-len)
              (db/import-into-infobright* conn (:table m) csv-file) ;; FIXME only mark files as imported if import evidently succeeded!
              (doseq [{table :table file :relative-file} (conj done m)]
                (db/mark-as-inserted conn table plant file))
              (.delete csv-file))
            (recur context' (conj done m) maps)))))))

