(ns ^{:doc "ETL pipeline for GlobaltechI daily 10min CSV files"}
    gt1-ui.data.etl
  (:require [clj-time
             [coerce :as tc]
             [core :as time]
             [format :as tf]
             [periodic :as tp]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [gt1-ui.data
             [availability :as avail]
             [csv :as csv]
             [db :as db]
             [metadata :as md]]
            [gt1-ui.bayes :as bayes]
            [gt1-ui.util.tree :as tree]
            [gt1-ui.lasso :as lasso]
            [org.clojars.smee.map :refer [map-values]]
            [org.clojars.smee.file :refer [find-files with-temp-file]]
            [streaming.protocols :as sp]
            [clj-time.periodic :as p]
            [timeseries.features :as feat])
  (:import [java.io File]
           [java.text Format SimpleDateFormat]
           [java.util Calendar Date Locale TimeZone]))

(defn sensor-paths [component-tree]
  (->> component-tree
       (mapcat (fn [{:keys [index children]}]
                   (for [{cid :index sensors :children} children,
                         {sid :index} sensors]
                     [index cid sid])))
       sort
       vec))

(defn components-series-formatter
  "Uses the ids in a component tree (refer to `gt1-ui.data.metadata/parse-structure`)
to create a function that can name columns in a normalized csv."
  [component-tree]
  (let [m (into {} (map (fn [[index cid sid]]
                          [[cid sid] index]) (sensor-paths component-tree)))]
    (fn [cid sid suffix]
      (str/join "/" [(m [cid sid]) cid sid suffix]))))

(defn add-series-names
"Give each leaf a new `:series` key with a name that contains all descendant indices."
  [component-tree]
  (loop [z (tree/tree-zip component-tree)]
     (cond
       (zip/end? z) (zip/root z)
       (empty? (zip/children z)) (-> z
                                     (zip/edit assoc :series (str/join "/" (conj (mapv :index (concat (filter map? (zip/path z)) [(zip/node z)])) "avg")))
                                     zip/next
                                     recur)
       :else (recur (zip/next z)))))


(defn csv-headers-to-column-spec
"Create a seq of column descriptions. Can be used to create db tables, store metadata etc."
  [plant component-tree column-names]
  (let [indexed-tree (tree/tree->indexed component-tree)]
    (for [cn column-names
          :let [[i j k type] (str/split cn #"/")
                mc (:label (get indexed-tree i))
                c (:label (get-in indexed-tree [i j]))
                node (get-in indexed-tree [i j k])]]
      (-> node
          (select-keys [:label :description :dimension :unit])
          (assoc :column-name cn
                 :plant plant
                 :component (str mc "/" c)
                 :statistics (or type "raw"))))))

(defn init-db! [conn db-name plant plant-data column-specs]
  (let [col-names (map :column-name column-specs)]
    (db/init-database! conn db-name)
    (db/create-tables! conn)
    (db/do! conn (db/create-series-table-command plant col-names))
    (db/do! conn (db/create-series-table-command (str plant "-traces") (filter #(.endsWith ^String % "avg") col-names)))
    (when (empty? (db/get-series-metadata conn plant))
      (db/insert-series-metadata! conn (map-indexed #(assoc %2 :table plant :index %1) column-specs)))
    (when (not (db/get-structure conn plant))
      (db/insert! conn :plants [plant-data]))))


(defn initialize-database!
"initialize db with plant specific tables, plant series metadata. Expects to find two files in `input-dir`: 'M5000-de.xml' with the plant's structure, 'coordinates.edn' with the plant coordinates.
This function is idempotent, can be run arbitrarily."
  [conn db-name input-dir]
  (let [coords (read-string (slurp (io/file input-dir "coordinates.edn")))
        tree (-> (io/file input-dir "M5000-de.xml")
                md/parse-structure
                (-> (update-in [1 :children 0 :children] conj
                               {:label "???" :index "59" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}
                               {:label "???" :index "60" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}
                               {:label "???" :index "61" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}))
                add-series-names)
        column-names (mapcat #(for [s ["avg" "min" "max" "sd"]] (str/join "/" (conj (vec %) s))) (sensor-paths tree))
        tree-string (pr-str tree)]
    (println "initializing database metadata, tables, ...")
    (dotimes [n 80]
      (let [plant (str "GT" (if (< n 9) "0" "") (inc n))
            [lat lon] (nth coords n)
            plant-data {:name plant :structure tree-string
                        :latitude lat :longitude lon}]
        (init-db! conn "gt1-db" plant plant-data (csv-headers-to-column-spec plant tree column-names))))    
    (println "done intializing database structures!")))

(def db-dateformat-full (tf/formatter "yyyy-MM-dd HH:mm:ss.SSS"))
(def dateformat-date (tf/formatter "yyyy-MM-dd"))
(def trace-dateformat (tf/formatter "yyyy-MM-dd-HH'h'mm'm'ss's'")
  )
(defn create-datetime-entries [date]
  [(tf/unparse db-dateformat-full date) ; sql timestamp
   (tf/unparse dateformat-date date) ; sql timestamp
   (tc/to-long date) ;unix timestamp
   (time/year date)
   (time/month date)
   0 ;; FIXME day of year
   (time/day date)
   (time/hour date)])

(defn create-infobright-csv
"Converts all csv files in a directory to csv files importable into infobright according
to the table spec in db.cljs
Needs the explicit order of column names. Will fill in missing columns with zeros."
  [plant column-names col-fn input-file out-file]
  (let [input-file (io/as-file input-file)
        out-file (io/as-file out-file)]
    (let [filename (.getName input-file)
          date-s (subs filename 0 (.indexOf filename ".txt"))
          date (tf/parse dateformat-date date-s)
          in (io/input-stream input-file)
          ;; skip header, always constant
          columns (csv/parse-10mwk-file in date col-fn)]
      (when (not-empty columns)
        ;; only run this if there is at least one valid column
        ;; else we will try to create unlimited rows because
        ;; we have no reference for column length
        (let [sorted-columns (for [cn (cons "timestamp" column-names)
                                  :let [col (get columns cn ::dummy)]]
                              (if (= col ::dummy)
                                (do ;(println "Warning: missing column" cn)
                                  (repeat 0))
                                col))
              rows (apply map vector sorted-columns)]
          (csv/write-csv out-file
                         (for [[date & vs] rows]
                           (into (create-datetime-entries date) vs)))))))
    out-file)


(defn create-event-csv
  [plant input-file output-file]
  (let [input-df (tf/formatter time/utc "yyyy-MM-dd-HH:mm:ss"  "yyyy-MM-dd-HH:mm:ss,SSS")]
    (with-open [rdr (-> input-file io/reader)]
      (let [rows (->> (clojure.data.csv/read-csv rdr :separator \;)
                      (map (partial take 22))
                      ;; the first few event files per power plant have too little columns
                      ;; the last three columns are irrelevant
                      (filter #(= 22 (count %))))]                
        (csv/write-csv output-file
                       (for [[^String date & vs] rows]
                         (into (create-datetime-entries (tf/parse input-df date)) (cons plant vs))))))
    output-file))

(defn create-parachange-csv
  [plant input-file output-file]
  (let [input-df (tf/formatter time/utc "yyyy-MM-dd-HH:mm:ss"  "yyyy-MM-dd-HH:mm:ss,SSS")
        nf (java.text.NumberFormat/getIntegerInstance)
        num #(.parse nf ^String %)
        df (java.text.NumberFormat/getNumberInstance)
        dbl #(.parse df ^String %)]
    (with-open [rdr (-> input-file io/reader)]
      (let [rows (->> (clojure.data.csv/read-csv rdr :separator \;)
                      (map (fn [[date user mode type component code old new]]
                             [(tf/parse input-df date)
                              user
                              (num mode)
                              (num type)
                              (num component)
                              (num code)
                              (case old
                                "TRUE" 1
                                "FALSE" 0
                                (dbl old))
                              (case new
                                "TRUE" 1
                                "FALSE" 0
                                (dbl new))])))]                
        (csv/write-csv output-file
                       (for [[date & vs] rows]
                         (into (create-datetime-entries date) (cons plant vs))))))
    output-file))

(defn create-trace-csv [plant column-names col-fn input-file output-file]
  (let [file (io/as-file input-file)
        n (.getName file)
        dir (.getParent file)
        [_ prefix date-str ms c err] (re-find #"M5000_(\d+)_(.*)_(\d\d\d\d)ms_KompNo-(\d+)_ErrNo-(\d+).trc.gz" n)
        timestamp (tf/parse trace-dateformat date-str)
        nf (java.text.NumberFormat/getNumberInstance)
        num #(.parse nf ^String %)
        columns (->> (for [interval [10 200 1000]
                           :let [file (io/file dir (format "M5000_%s_%s_%04dms_KompNo-%s_ErrNo-%s.trc.gz" prefix date-str interval c err))]
                           :when (.exists file)
                           :let [timestamps (tp/periodic-seq (time/minus timestamp (time/millis (* 300 interval)))
                                                             (time/plus timestamp (time/millis (* 100 interval)))
                                                             (time/millis interval))]]                       
                       (with-open [rdr (io/reader (java.util.zip.GZIPInputStream. (io/input-stream file)))]
                         (let [[[_ n] & rows] (clojure.data.csv/read-csv rdr :separator \;)
                               event-idx (num n)]
                           (into {:timestamps timestamps}
                                 (for [[type c s divisor & vs] rows
                                       :when (= type "aK")
                                       :let [divisor (double (num divisor))
                                             series-name (col-fn c s "avg")
                                             vs (map num vs)
                                             vs (vec (take 400 (drop event-idx (cycle (map #(/ % divisor) vs)))))]]
                                   [series-name vs])))))
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
        sorted-columns (for [cn (cons :timestamps column-names)
                             :let [col (get columns cn ::dummy)]]
                         (if (= col ::dummy)
                           (do ;(println "Warning: missing column" cn)
                             (repeat 0))
                           col))
        rows (apply map vector sorted-columns)]
    (csv/write-csv output-file
                   (for [[date & vs] rows]
                     (into (create-datetime-entries date) vs)))))

(defn- date-only [dt]
  (let [dt (tc/to-date-time dt)]
    (time/date-midnight (time/year dt) (time/month dt) (time/day dt))))

(defn reconstruct-error-intervals [conn import-dir]
  (let [file (io/file import-dir (str "intervals" (java.util.UUID/randomUUID)))]
    (with-open [bw (io/writer file)]
      (let [write! (fn [start end plant component error-id]
                     (.write bw (str/join ";"
                                  (into (create-datetime-entries (tc/to-date-time start))
                                        [(tf/unparse db-dateformat-full (tc/to-date-time end))
                                         plant component error-id])))
                     (.write bw "\n"))
            m (db/process-all conn "events" ["timestamp" "plant" "component" "error-id"]
                              (fn [rows]
                                (reduce (fn [m {:keys [timestamp component plant error-id]}]
                                          (let [{:keys [start id c]} (m plant)]
                                            (cond
                                              (and (not id) (not= 0 error-id)) (assoc m plant {:start timestamp :id error-id :c component})
                                              (and id (not= id error-id)) (do
                                                                            (write! start timestamp plant c id)
                                                                            (dissoc m plant))
                                              :else m)))
                                        {} rows)))]
        (doseq [[plant {:keys [start id c]}] m]
          (write! start (time/today-at-midnight) plant c id))))
    (db/do! conn "drop table if exists errors")
    (db/create-tables! conn)
    (db/import-into-infobright* conn "errors" file)
    (.delete file)))

;;;;;;;;;;;;;;;; main ETL logic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                    "DataParaChange" "parachanges"
                    "DataTrace" (str plant "-traces")} type)})))

(defn find-importable-files
"Find all text files we can handle and are not yet imported in the database."
  [conn input-dir]
  (let [newest-imported-file (reduce (fn [res m] (assoc res [(:table m) (:plant m)] (:filename m)))
                                     {}
                                     (db/adhoc conn "select `table`,plant,max(filename) as filename from imported group by plant,`table`"))]
    (for [file (filter #(re-matches file-pattern (str %)) (file-seq (io/as-file input-dir)))
          :let [{table :table plant :plant :as res} (file-metadata file)]
          :when (and res (pos? (compare (str file) (get newest-imported-file [table plant]))))]
      res)))

(defn convert [{:keys [plant file type table]} col-fn column-names import-dir csv-file]
  (case type
    "Data10MinMW" (create-infobright-csv plant column-names col-fn file csv-file)
    "DataOpMode"  (create-event-csv plant file csv-file)
    "DataParaChange" (create-parachange-csv plant file csv-file)
    "DataTrace" (create-trace-csv plant (filter #(.endsWith ^String % "avg") column-names) col-fn file csv-file)))

(defn import! [conn file-maps import-dir]
  (let [column-names (drop (count db/date-dimensions) (db/table-columns conn "GT01")) ;; every plant's table has the same structure
        column-names-map (reduce (fn [m n]
                                   (let [[_ a b c d] (re-find #"(\d+)/(\d+)/(\d+)/(.*)" n)]
                                     (assoc m [b c d] n)))
                                 {} column-names)
        col-fn (fn [cid sid suffix]
                 (get column-names-map [cid sid suffix]))
        grouped (group-by (juxt :plant :type) file-maps)]
    (doseq [[[plant type] maps] (into (sorted-map) grouped)
            :let [maps (sort-by (comp str :file) maps)]
            chunk (partition-all 60 maps)
            :let [fd (tf/unparse dateformat-date (:date (first chunk)))
                  ld (tf/unparse dateformat-date (:date (last chunk)))
                  csv-file (io/file import-dir (format "%s-%s-%s-%s.csv" plant type fd ld))]]
      (println plant type fd ld)
      ;; create big csv file for infobright
      (doseq [m chunk]
        (convert m col-fn column-names import-dir csv-file))
      (db/import-into-infobright* conn (:table (first chunk)) csv-file)
      (doseq [{table :table file :file} chunk]
        (db/mark-as-inserted conn table plant file))
      (.delete csv-file))))

;;;;;;;;;;;;;; TRACE data validation - there seem to be inconsistencies between 10min and trace data ;;;;;;;;;;;;;;;;;

(defn validate-traces
"Check, if there are discrepancies between 10min data and trace data per sensor (scaling etc.)"
  [conn]
  (let [plants (take 1 (map #(format "GT%02d" (inc %)) (range 80)))
        column-names (drop (count db/date-dimensions) (db/table-columns conn (first plants)))
        avg-names (filter #(.contains % "/avg") column-names)
        cs (str/join "," (map db/entities column-names))]
    (for [plant plants
          :let [start (tc/from-long (:timestamp (first (db/adhoc conn (format "select min(unixtimestamp) as timestamp from `%s-traces` where timestamp>?" plant) (time/date-time 2015 9 26)))))
                stats (zipmap avg-names (repeatedly #(streaming.RunningStats.)))                
                agg (map-values first (db/raw-value-in-time-range conn plant column-names [[start (time/plus start (time/minutes 10))]]))]]
      (do
        (println plant start)
        (doseq [row (filter #(zero? (mod (:unixtimestamp %) 1000)) (db/adhoc conn (format "select * from `%s-traces` where timestamp>=? order by timestamp limit 1100" plant) start)),
                col avg-names]
          (.push ^streaming.RunningStats (get stats col) (get row (keyword col))))
        (into (sorted-map)
              (map (fn [[^String k ^streaming.RunningStats s]]
                     (let [prefix (subs k 0 (.lastIndexOf k "/"))]
                       [k {:trace  {:avg (.getMean s)
                                    :min (.getMin s)
                                    :max (.getMax s)
                                    :sd (.getStandardDeviation s)}
                           :rs (bean s)
                           :timestamp start
                           :agg {:avg (get agg (str prefix "/avg"))
                                 :min (get agg (str prefix "/min"))
                                 :max (get agg (str prefix "/max"))
                                 :sd  (get agg (str prefix "/sd"))}}])) stats))))))
(comment
  (defn validated-traces-table [v]
   (clojure.pprint/print-table [:name :label
                                :10min-avg :trace-avg
                                :10min-sd :trace-sd
                                :10min-min :trace-min
                                :10min-max :trace-max]
                               (map (fn [[k v]] {:name k
                                                :label (gt1-ui.local.lasso-experiments/name->label k)
                                                :10min-avg (-> v :agg :avg)
                                                :10min-sd (-> v :agg :sd)
                                                :10min-min (-> v :agg :min)
                                                :10min-max (-> v :agg :max)
                                                :trace-avg (-> v :trace :avg)
                                                :trace-sd (-> v :trace :sd)
                                                :trace-min (-> v :trace :min)
                                                :trace-max (-> v :trace :max)}) v)))
  (def m (validate-traces gt1-ui.handler/conn))
  (spit "d:/tbl.csv" (with-out-str (validated-traces-table (first m)))))
;;;;;;;;;;;;; run anomaly detection algorithms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- midnight? [ts]
  (every? #(zero? (% ts)) [time/hour time/minute time/second]))

(defn run-algorithms! [conn]
  (let [today (time/today-at-midnight)
        fmt (partial tf/unparse (tf/formatters :basic-date))
        net (bayes/load-net)] 
    (doseq [{:keys [id plant algorithm options learn-end]} (db/algorithms conn)
            ;:when (= plant "GT04")
            :let [_ (println (format "model id=%d for plant %s" id plant))
                  probability (get-in options [:thresholds :probability] 0.7)
                  model (sp/dehydrate algorithm)
                  res (db/algorithm-state conn id)                  
                  state (or (get res :state) (lasso/initial-state algorithm options))
                  last-date (or (get res :last-update) learn-end)
                  ;; we processed the data until `last-date`. Since we use lagging features we need
                  ;; to drop n rows before continuing the processing. So we need to load data
                  ;; including the last two measurements before the last processed timestamp.
                  ;; we have 10min data, so let's drop (maxLag-1)*10min.
                  start-time (time/minus last-date (time/minutes (* 10 (max 0 (dec (:max-lag options))))))
                  _ (println (format "dates: %s - %s" (fmt start-time) (fmt today)))
                  time-intervals (avail/operation-time-intervals conn plant start-time today)                  
                  ingester (fn [state row]
                             (let [next-state (lasso/ingest-new-measurement algorithm state row)
                                   ts (get row "timestamp")]
                               (when (midnight? ts) ;; new day, store current anomaly candidates. FIXME: will break if there is no date exactly at midnight
                                 
                                 (let [candidates (lasso/anomaly-candidates-v1 model state options)
                                       faults (lasso/identify-faults candidates options)
                                       date (time/date-time (time/year ts) (time/month ts) (time/day ts))]
                                   (println ts (count candidates))
                                   (when (not-empty faults)
                                     (clojure.pprint/pprint faults))
                                   (db/store-algorithm-result! conn plant id date {:candidates candidates
                                                                                   :anomalies faults})))
                               next-state))
                  series-names (set (mapcat feat/find-stems (sp/input-names algorithm)))
                  new-state (if (not-empty time-intervals)
                              (lasso/apply-lasso-models conn
                                                        series-names
                                                        ingester
                                                        state
                                                        plant
                                                        time-intervals
                                                        options) state)
                  last-date (or (second (last time-intervals)) last-date)]] 
      (db/store-algorithm-state! conn plant id last-date new-state))))

(comment
  (def tree (-> "e:/datasets/gt1/metadaten/M5000-de.xml"
                md/parse-structure
                (-> (update-in [1 :children 0 :children] conj
                               {:label "???" :index "59" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}
                               {:label "???" :index "60" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}
                               {:label "???" :index "61" :dimension "???" :unit "???" :description "vorhanden seit dem 05.12.2015"}))
                add-series-names))
  
  (def parsed (->> tree
                   components-series-formatter
                   (csv/parse-10mwk-file "e:/datasets/gt1/DATA_GTI/Downloaded_ProcessedData/GT79/Data10MinMW/2016-02-22.txt" #inst "2016-02-22")))

  (def column-names (sort (keys (dissoc parsed "timestamp"))))
  
  ;; TODO automatically initialize database
  (def conn (or (db/connection "remote")
                (db/create-connection-pool! "remote" (db/default-spec "remote" "mysql://root:c5bf19d0918e02c1@localhost:27572/gt1-db"))))
  (def conn (or (db/connection "local")
                (db/create-connection-pool! "local" (db/default-spec "local" "mysql://root@localhost:5029/gt1-db"))))
  (db/clear-db! conn)

  (initialize-database! conn "gt1-db" "e:/datasets/gt1/DATA_GTI/Downloaded_ProcessedData")
)


