(ns gt1-ui.data.db
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.jdbc :as sql] 
            [org.clojars.smee.map :refer [map-values]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [gt1-ui.util :refer [kw-name]]
            [streaming.fading :as fading])
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:import [java.sql PreparedStatement]))

;; automatically convert dates/timestamps from/to JODA
(extend-protocol sql/IResultSetReadColumn                                
  java.sql.Date
  (result-set-read-column [v _ _] (t/plus (tc/from-sql-date v) (t/hours 1)));;clojure.jdbc uses .getDate with the default calendar, which results in a date instance in the local timezone. Convert it back to UTC the manual way...                    

  java.sql.Timestamp
  (result-set-read-column [v _ _] (tc/from-sql-time v)))

(extend-type org.joda.time.DateTime                                       
  sql/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setTimestamp stmt idx (tc/to-sql-time v))))          

(extend-type org.joda.time.DateMidnight                                  
  sql/ISQLParameter
  (set-parameter [v ^PreparedStatement stmt idx]
    (.setDate stmt idx (tc/to-sql-date v))))

(defn entities
"MySQL specific quoting."
  [s]
  (str "`" s "`"))

;;;;;;;;;;;;;;; connection pools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce ^:private connection-pools (atom {}))

(defn- create-db-connection-pool
  [{:keys [classname connection-name parameters connection-uri]}]
  (let [{:keys [userInfo host scheme port query path]} (bean (java.net.URI. connection-uri))
        [user password] (str/split userInfo #":")
        url (format "jdbc:mysql://%s:%d%s" host port path)
        cpds (doto (ComboPooledDataSource. (or connection-name (str (java.util.UUID/randomUUID))))
               (.setDriverClass classname)
               (.setJdbcUrl url)
               (.setProperties (reduce (fn [p [k v]] (.setProperty p (str k) (str v)) p) (java.util.Properties.) parameters))
               (.setUser user)
               (.setPassword password)
               ;; expire excess connections after 30 minutes of inactivity:
               (.setMaxIdleTimeExcessConnections (* 1 60))
               (.setMaxConnectionAge (* 1 60))
               (.setIdleConnectionTestPeriod (* 1 30))
               ;; expire connections after 3 hours of inactivity:
               (.setMaxIdleTime (* 3 60 60)))
        ds {:datasource cpds}]
    ;; ensure that MySQL does no funny business regarding transformation of dates/timestamps
    (sql/db-do-commands ds ["SET GLOBAL time_zone = '+0:00'"
                            "SET SESSION time_zone = '+0:00'"]) 
    ds))

(defn default-spec [conn-name db-url]
  {:classname "com.mysql.jdbc.Driver"
   :connection-uri db-url
   :connection-name conn-name
   :parameters {"useCursorFetch" true
                "defaultFetchSize" 100000
                "useLegacyDatetimeCode" false
                "noTimezoneConversionForDateType" true
                "serverTimezone" "UTC"}})

(declare init-database!)

(defn create-connection-pool! [db-name spec]
  (let [conn (create-db-connection-pool spec)]
    (init-database! conn db-name)    
    (swap! connection-pools assoc db-name (with-meta conn {:db-spec spec}))
    conn))

(defn connection [db-name]
  (get @connection-pools db-name))

;;;;;;;;;;;;;; table definitions and DDL handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn- mysql-table
"Create 'CREATE TABLE IF NOT EXISTS' DDL statement for MySQL (using the correct `-quoting
characters for each identifier)."
  ([table-name col-specs] (mysql-table table-name col-specs {}))
  ([table-name col-specs opts]
   (let [q (sql/create-table-ddl table-name col-specs (assoc opts :entities entities))
         [empty-string suffix] (str/split q #"CREATE TABLE")]
     (assert (empty? empty-string))
     (str "CREATE TABLE IF NOT EXISTS" suffix))))

(def date-dimensions [[:timestamp "datetime"] 
                      [:date "date"]
                      [:unixtimestamp "bigint"]           
                      [:year "smallint"]
                      [:month "tinyint"]
                      [:day_of_year "smallint"]
                      [:day_of_month "tinyint"]
                      [:hour_of_day "tinyint"]])

#_(defn table-columns
    "Bug in infobright: 12 columns are missing in table GT01, and only there...."
    [conn table-name]
  (let [db-name (sql/query conn ["select database() as db"] :row-fn :db :result-set-fn first)] (mapv :cn (sql/query conn ["SELECT `COLUMN_NAME` as cn FROM `INFORMATION_SCHEMA`.`COLUMNS`
                            WHERE `TABLE_SCHEMA`=? AND `TABLE_NAME`=? ORDER BY `ORDINAL_POSITION`"
                                                                                                                          db-name table-name]))))

(defn table-columns [conn table-name]
  (sql/query conn [(str "EXPLAIN " (entities table-name))] {:result-set-fn (partial mapv :field)}))

(defn create-series-table-command [name headers]
  (mysql-table
   name
   (into date-dimensions
         (for [hdr headers]
           [hdr "double"]))))

(defn create-tables! [conn]
  (sql/db-do-commands conn
     [(mysql-table :plants
                   [[:name "varchar(127)"]
                    [:latitude "double"]
                    [:longitude "double"]
                    [:structure :text]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :tables
                   [[:plant "varchar(127)"]
                    [:start "timestamp"]
                    [:end "timestamp"]
                    [:table-name "varchar(127)"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :series 
                   [[:plant "varchar(127)"]       ;name of the power plant
                    [:index "int"]                ; index of column in plant specific table
                    [:column-name "varchar(127)"] ;name of the series 
                    [:label "varchar(127)"]
                    [:table "varchar(127)"]
                    [:description :text]
                    [:unit "varchar(127)"] ;physical SI unit
                    [:dimension "varchar(127)"] ;physical entity, measured in :unit
                    [:component "varchar(127)"] ;name of the origin component
                    [:statistics "varchar(30)"] ; raw,min,max,sd,mean...
                    ]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :imported
                   [[:filename "varchar(1024)"]
                    [:table "varchar(127)"]
                    [:plant "varchar(127)"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :events
                   (into date-dimensions
                         [[:plant "varchar(20)" "comment 'lookup'"]
                          [:user "varchar(100)" "comment 'lookup'"]
                          [:old-mode "smallint"]
                          [:new-mode "smallint"]
                          [:brake-mode "integer"]
                          [:component "integer"]
                          [:error-id "integer"]
                          [:power "double"]
                          [:wind-speed "double"]
                          [:wind-direction "double"]
                          [:nacelle-position "double"]
                          [:generator-rpm "double"]
                          [:pitch-angle "double"]
                          [:aux-energy "double"]
                          [:motor-status1  "integer"]
                          [:motor-status2 "integer"]
                          [:motor-status3 "integer"]
                          [:motor-status4 "integer"]
                          [:motor-status5 "integer"]
                          [:pls-status1 "integer"]
                          [:pls-status2 "integer"]
                          [:pls-status3 "integer"]]))
      (mysql-table :errors ;; explicit representation of error intervals
                   (into date-dimensions
                         [[:end "datetime" "default 0"]
                          [:plant "varchar(20)" "comment 'lookup'"]
                          [:component "integer"]
                          [:error-id "integer"]]))
      (mysql-table :parachanges
                   (into date-dimensions
                         [[:plant "varchar(20)" "comment 'lookup'"]
                          [:user "varchar(100)" "comment 'lookup'"]
                          [:mode "smallint"]
                          [:type "smallint"] ;{1 :digital 2 :analog 3 :suppressed 4 :delay}
                          [:component "integer"]
                          [:channel "integer"]
                          [:new "double"] ; mapping TRUE to 1 and FALSE to 0
                          [:old "double"]]))
      ;; stateful tables, may change contents

      (mysql-table :algorithms
                   [[:id "integer" "primary key" "auto_increment"]
                    [:plant "varchar(127)"]                   
                    [:learn-start "datetime"]
                    [:learn-end "datetime"]
                    [:algorithm "mediumtext"]
                    [:active "boolean"]
                    [:options "text"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :algorithm-states
                   [[:algorithm "integer"]
                    [:plant "varchar(127)"]                   
                    [:last-update "datetime"]
                    [:state "longtext"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :algorithm-results
                   [[:algorithm "integer"]
                    [:plant "varchar(127)"]
                    [:timestamp "datetime"]
                    [:results "mediumtext"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :annotations
                   [[:algorithm "integer"]
                    [:plant "varchar(127)"]
                    [:timestamp "datetime"]
                    [:editted "datetime"]
                    [:text "mediumtext"]]
                   {:table-spec "engine = 'MyIsam'"})]))

(defn clear-db! [conn]
  (doseq [table (mapcat vals (sql/query conn "show tables"))]
    (sql/db-do-commands conn [(str "drop table " (entities table))])))

(defn init-database! [conn db-name]
  (sql/db-do-commands conn [(str  "create database if not exists " (entities db-name))])
  (create-tables! conn))

;;;;;;;;;;;;;;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn import-into-infobright* [conn table & data-csv-files]
  (sql/db-do-commands
   conn
   (cons "set @bh_dataformat = 'txt_variable'"
         (for [file data-csv-files] 
           (format "load data infile '%s' into table `%s` fields terminated by ';'"  (.replaceAll (str file) "\\\\" "/") table)))))

(defn mark-as-inserted [conn table-name plant file-name]
  (sql/insert! conn "imported" {:table (str table-name) :filename (str file-name) :plant (str plant)} {:entities entities}))

(defn insert! [conn table rows]
  ; sql/insert! doesn't take collections of rows but rather arbitrary arguments...
  (sql/insert-multi! conn table rows {:entities entities}))

(defn insert-series-metadata! [conn column-specs]
  (sql/insert-multi! conn :series column-specs {:entities entities}))
;;;;;;;;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adhoc "Adhoc query, for development" [conn query & params]
  (sql/query conn (apply vector query params)))

(defn do! "Run adhoc commands, like drop, create table, etc." [conn & cmds]
  (sql/db-do-commands conn cmds))

(defn imported? [conn table filename]
  (= 1 (:count (first (sql/query conn ["select count(*) as count from `imported` where `table`=? and `filename`=?" table (str filename)])))))

;;;;;;;;; plant metdata ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-column-names [conn plant]
  (sql/query conn ["select `column-name` from series where plant=? order by `index`" plant] {:row-fn :column-name}))
(defn get-plant-metadata [conn plant]
  (-> conn
      (sql/query ["select name,structure as tree from plants where name=?" plant])
      first
      (update :tree edn/read-string)))

(defn get-series-metadata [conn plant]
  (sql/query conn ["select * from series where plant=?" plant]))

(defn get-all-series-metadata [conn]
  (sql/query conn ["select * from series"]))

(defn get-structure [conn plant-name]
  (-> conn
      (sql/query ["select structure from plants where name=?" plant-name])
      first
      :structure
      edn/read-string))

;;;;;;;;; measurement data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mean [vs]
  (let [c (double (count vs))]
    (if (> c 0)
      (/ (reduce + vs) c)
      0)))

(defn- insert-nils 
  "Dygraph needs explicit null values if there should be a gap in the chart.
Tries to estimate the average x distance between points and whenever there is a gap bigger than two times
that distance, inserts an artificial entry of shape `[(inc last-x)...]`
Use this function for all dygraph data."
  [data]
  (if (empty? data)
    nil
    (let [gaps (->> data (map (comp tc/to-long first)) (partition 2 1) (map (fn [[a b]] (- b a))))
          max-gap (* 5 (mean gaps)) 
          v1 (second (first data))
          nothing (if (coll? v1) (vec (repeat (count v1) Double/NaN)) Double/NaN)] 
         (conj
          (reduce (fn [res [[x1 & vs :as orig] [x2 _]]] 
                    (let [diff (- (tc/to-long x2) (tc/to-long x1))
                          res (conj res orig)] 
                         (if (> diff max-gap)
                           (conj res (apply vector (tc/to-sql-time (inc (tc/to-long x1)))
                                            (repeat (count vs) nothing)))
                           res)))
                  []
                  (partition 2 1 data))
          (last data)))))

(defn rolled-up-values-in-time-range 
  "Find min, max, and average of values aggregated into `num` time slots."
  [conn tables columns start-time end-time num]
  (let [s (tc/to-long start-time) 
        e (tc/to-long end-time)
        columns (for [^String c columns] (subs c 0 (.lastIndexOf c "/")))
        num (max 1 num)
        order (into {} (map-indexed (comp vec reverse vector) tables))
        interval-in-s (max 1 (long (/ (- e s) num)))
        params (for [c columns] (format "avg(`%1$s/avg`) as `%1$s/avg`, min(`%1$s/min`) as `%1$s/min`, max(`%1$s/max`) as `%1$s/max`" c))
        queries (for [table tables] (format "(select '%s' as plant, timestamp, %s from %s where timestamp >= ? and timestamp < ? group by (unixtimestamp div ?))" table (str/join "," params) table))
        query (format "select * from (%s) as combined order by combined.timestamp" (str/join " UNION ALL " queries))]
    (sql/query conn (apply vector query
                           (take (* 3 (count tables)) (cycle [start-time end-time interval-in-s])))
               {:result-set-fn (fn [rows]
                                 (->> rows
                                      (partition-by :timestamp)
                                      (map (partial reduce #(assoc % (:plant %2) %2) {}))
                                      (mapv (fn [rows-m]
                                              ;; iterate over partitions of same time instant
                                              ;; put triples of [min avg max] in order of plants and columns
                                              ;; replace each missing value with NaN
                                              (loop [res [(:timestamp (first (vals rows-m)))], tables tables]
                                                (let [row (get rows-m (first tables))]
                                                  (cond
                                                    (nil? tables) res
                                                    (nil? row) (recur (into res (repeat (count columns) Double/NaN)) (next tables))
                                                    :else (recur (into res (for [name columns
                                                                                 :let [avg (get row (keyword (str name "/avg")))
                                                                                       min (get row (keyword (str name "/min")))
                                                                                       max (get row (keyword (str name "/max")))]]
                                                                             [min avg max]))
                                                                 (next tables)))))))))})))

(defn traces
  "Query full trace data. Needs to query `unixtimestamp` fields because in Infobright
DATETIME does not support milliseconds."
  [conn plants columns start-time end-time]
  (let [params (str/join "," (map entities columns))
        queries (for [plant plants] (format "(select '%s' as plant, unixtimestamp as timestamp, %s from `%s` where timestamp >= ? and timestamp < ?)" plant params (str plant "-traces")))
        query (format "select * from (%s) as combined order by combined.timestamp" (str/join " UNION ALL " queries))
        order (into {} (map-indexed (comp vec reverse vector) plants))]
    (sql/query conn (apply vector query (take (* 2 (count plants)) (cycle [start-time end-time])))
               {:row-fn (fn [m] (update m :timestamp tc/to-date-time))
                :result-set-fn (fn [rows]
                                 (->> rows
                                      (partition-by :timestamp)
                                      (map (partial sort-by (comp order :plant)))
                                      (mapv (fn [rows]
                                              ;; iterate over partitions of same time instant
                                              ;; put triples of [min avg max] in order of plants and columns
                                              ;; replace each missing value with nil
                                              (loop [res [(:timestamp (first rows))], rows rows, plants plants]
                                                (let [row (first rows)]
                                                  (cond
                                                    (nil? plants) res
                                                    (nil? row) (recur (into res (repeat (count columns) nil)) nil (next plants))
                                                    :else (recur (into res (for [name columns
                                                                                 :let [avg (get row (keyword name))]]
                                                                             [avg avg avg]))
                                                                 (next rows)
                                                                 (next plants)))))))))})))

;;;;;;;;;;;;;; functions to process many/all rows in a table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn process-raw-values-in-time-range
  "Applies a function `func` to each result row. Each row's key is a string, not a keyword.
This function can be used to stream multiple sensor measurements per timestamp without 
loading all data into memory."
  [conn table columns time-intervals func]
  {:pre [(every? string? columns)
         (not-empty columns)
         (not-empty time-intervals)
         (every? #{2} (map count time-intervals))]}
  (let [params (map entities columns)
        times (repeat (count time-intervals) "(timestamp >= ? and timestamp < ?)")
        query (format "select timestamp, %s from %s where (%s) order by timestamp" (str/join "," params) table (str/join " or " times))]
    (sql/query conn (apply vector query (reduce into time-intervals))
               {:row-fn #(map-values kw-name identity %)
                :result-set-fn func})))


(defn raw-value-in-time-range
  "All values for each column, returns map of column to values in time order."
  [conn table columns time-intervals]
  {:pre [(every? string? columns)
         (not-empty columns)
         (not-empty time-intervals)
         (every? #{2} (map count time-intervals))]}
  (process-raw-values-in-time-range conn table columns time-intervals
     (fn [rows] ;; FIXME should really return individual vectors per time interval
       (let [cols (into {"timestamp" []} (map vector columns (repeat (vector-of :double))))]
         (reduce (fn [cs row]
                   (reduce (fn [cs c] (update cs c conj (get row c)))
                           cs (keys row)))
                 cols rows)))))

(defn process-all
"Select all rows in a table. Streams rows through function `fun`."
  [conn table columns fun]
  (sql/query conn [(format "select %s from %s order by timestamp" (str/join "," (map entities columns)) table)] {:result-set-fn fun}))

;;;;;;;;;;;;;;; modes and errors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn plant-mode-changes
"Return all operation mode changes in a time interval for plants."
  [conn start-time end-time]
  (let [end-time (t/plus (tc/to-date-time end-time) (t/seconds 1))
        query "select * from (
    (select plant, min(timestamp) as timestamp,`old-mode`,`new-mode` from events where timestamp between ? and ? group by plant order by timestamp) 
    UNION ALL (select plant,timestamp, `old-mode`,`new-mode` from events where timestamp between ? and ? and `new-mode` != `old-mode` order by timestamp ASC)
    UNION ALL (select plant, max(timestamp) as timestamp,`old-mode`,`new-mode` from events where timestamp between ? and ? group by plant order by timestamp ASC)  
 ) t 
 order by t.plant, timestamp;"
        params [start-time end-time
                start-time end-time
                start-time end-time]]
    (sql/query conn (apply vector query params)
               {:result-set-fn (fn [rows]
                                 (->> rows
                                      (partition-by identity) ;; TODO wtf?
                                      (map first)
                                      (group-by :plant)
                                      (reduce-kv (fn [m plant rows]
                                                   (assoc m plant (mapv #(dissoc % :plant) rows)))
                                                 {})))})))



(defn errors
  "Find error intervals for power plants. Returns 
  `{plant [{:timestamp date, :component num, :error-id num, :end date}]}`"
  [conn plants start-time end-time]
  (let [subquery (str/join " or "(repeat (count plants) "plant = ?"))
        s start-time
        e end-time]
    (sql/query conn (apply vector (str "select plant, timestamp, end, `component`, `error-id` from errors where (timestamp between ? and ? or end between ? and ?) and (" subquery ")") s e s e plants)
               {:result-set-fn (fn [rows]
                                 (reduce (fn [m row] (let [plant (:plant row)]
                                                      (update m plant (fnil conj []) (dissoc row :plant))))
                                         {}
                                         rows))})))

;;;;;;;;;;;;;; anomaly analysis CRUD ;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- str->clj [key]
  (fn [m] (update m key read-string)))

(defn- serialize-to-string [clj-obj]
  (binding [*print-length* nil *print-level* nil *print-dup* true] (pr-str clj-obj)))

;;;;;;;;;;;; algorithms/models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn algorithms [conn]
  (sql/query conn ["select * from algorithms where active=true"] {:row-fn (comp (str->clj :options) (str->clj :algorithm)) :result-set-fn vec}))

(defn algorithm [conn plant]
  (sql/query conn ["select id, algorithm, options from algorithms where active=true and plant=?" plant] {:row-fn (comp (str->clj :options) (str->clj :algorithm)) :result-set-fn first}))

(defn store-algorithm!
"Serialize model, store in DB, return generated ID."
  [conn plant from to model options]
  (let [text (serialize-to-string model)
        opts (serialize-to-string options)]
    (:generated_key (first (sql/insert! conn :algorithms {:plant plant
                                                          :algorithm text
                                                          :options opts
                                                          :learn-start from
                                                          :active true
                                                          :learn-end to}
                                        {:entities entities})))))

;;;;;;;;;;;; persistent algorithm processing state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn algorithm-state [conn algorithm-id]
  (sql/query conn ["select state,`last-update` from `algorithm-states` where algorithm=?" algorithm-id] {:row-fn (str->clj :state) :result-set-fn first}))

(defn store-algorithm-state! [conn plant algorithm-id last-processed-timestamp state]
  (if (zero? (sql/query conn ["select count(*) as c from `algorithm-states` where `algorithm`=? and plant=?" algorithm-id plant] {:row-fn :c :result-set-fn first}))
    (sql/insert! conn :algorithm-states {:algorithm algorithm-id :plant plant :last-update last-processed-timestamp :state (serialize-to-string state)} {:entities entities})
    (sql/update! conn :algorithm-states {:last-update last-processed-timestamp :state (serialize-to-string state)} ["algorithm=? and plant=?" algorithm-id plant] {:entities entities})))

;;;;;;;;;;; daily algorithm results for users ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn algorithm-results [conn plant]
  (sql/query conn ["select timestamp, results from `algorithm-results` where plant=? order by timestamp ASC" plant]
             {:row-fn (str->clj :results)
              :result-set-fn (fn [rows]
                               (first
                                (reduce (fn [[res state] {ts :timestamp {anomalies :anomalies :as results} :results}]
                                          (let [[as state] (reduce-kv (fn [[m state'] sensor n]
                                                                        (let [t (get state sensor ts)]
                                                                          [(assoc m sensor {:n n :since t})
                                                                           (assoc state' sensor t)]))
                                                                      [{} {}] anomalies)]
                                            [(conj res {:timestamp ts :results (assoc results :anomalies as)})
                                             state]))
                                        [[] {}] rows)))}))

(defn store-algorithm-result! [conn plant algorithm-id date results]
  (let [text (binding [*print-length* nil *print-level* nil *print-dup* true] (pr-str results))]
    (sql/insert! conn :algorithm-results {:results (serialize-to-string results) :timestamp date :plant plant :algorithm algorithm-id} {:entities entities})))


;;;;;;;;;;;;;;;;;;;;;; annotations ;;;;;;;;;;;;;;;;;;;;;;;
(defn store-annotation! [conn plant day text]
  (sql/insert! conn :annotations {:plant plant :timestamp day :editted (t/now) :text text}))

(defn annotations [conn plant]
  (sql/query conn ["select timestamp, text from annotations where editted in (select max(editted) from annotations where plant=? group by timestamp)" plant]
             {:result-set-fn (fn [rows]
                               (let [rows (filter #(not-empty (:text %)) rows)]
                                 (zipmap (map :timestamp rows) (map :text rows))))}))
;;;;;;;;;;;;;;;;;;; specific queries ;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-wind-stats [conn plants from to]
  ;; relative wind direction 2/14/13
  ;; nacelle direction 2/13/10
  ;; wind speed 1+2 2/14/12
  (let [qs (for [p plants] (format "select '%1$s' as plant, (`2/14/13/avg`+`2/13/10/avg`) as `dir`, `2/14/12/avg` as `speed` from `%1$s` where timestamp >= ? and timestamp < ?" p))
        query (str "select plant,dir,speed from ("(str/join " UNION ALL " qs) ") A")
        bin-fn (binning/bounded-bin-fn 0 360 30)
        all-bins (mapv first (distinct (map bin-fn (range 360))))]
       (sql/query conn (apply vector query (take (* 2 (count plants)) (cycle [from to])))
                  {:result-set-fn (fn [rows]
                                    (->> rows
                                         (reduce (fn [m {:keys [plant dir speed]}]
                                                   (let [[bin-start] (bin-fn (mod dir 360))
                                                         avg (get-in m [plant bin-start])]
                                                     (assoc-in m [plant bin-start] (avg speed))))
                                                 (zipmap plants (repeat (zipmap all-bins (repeat (fading/average 1.0))))))
                                         (map-values (partial map-values deref))))})))

(defn fetch-wind-stats [conn plants from to]
  ;; relative wind direction 2/14/13
  ;; nacelle direction 2/13/10
  ;; wind speed 1+2 2/14/12
  (let [sector-angle 15
        |sectors| (/ 360 sector-angle)
        qs (for [p plants] (format "select '%1$s' as plant, mod(mod(mod(`2/14/13/avg`+`2/13/10/avg`,360)+360,360) div %2$d,%3$d) as `dir`, avg(`2/14/12/avg`) as `speed`, count(*) as n from `%1$s` where timestamp >= ? and timestamp < ? group by dir" p sector-angle |sectors|))
        query (str "select plant,dir,speed,n from ("(str/join "\n UNION ALL " qs) ") A")
        m (zipmap plants (repeat (into (sorted-map) (zipmap (range 0 360 sector-angle) (repeat {:n 0 :v 0})))))]
       (sql/query conn (apply vector query (take (* 2 (count plants)) (cycle [from to])))
                  {:result-set-fn (fn [rows]
                                    (let [m (reduce #(assoc-in %1 [(:plant %2) (* sector-angle (:dir %2))] {:n (:n %2) :v (:speed %2)}) m rows)]
                                      (map-values (fn [sectors]
                                                    (let [s (double (apply + (map :n (vals sectors))))]
                                                      (if (zero? s)
                                                        sectors
                                                        (map-values #(update % :n / s) sectors)))) m)))})))
;;;;;;;;;;;;;;;;;; experiments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn aggregation-of
  "Assuming the sample sizes are equal. Refer to Wikipedia, sample deviation"
  [& args]
  (let [n (double (count args))
        sds  (map :sd args)
        means (map :mean args)
        new-mean (/ (apply + means) n)]
    {:mean new-mean
     :min (apply min (map :min args))
     :max (apply max (map :max args))
     :sd (Math/sqrt (- (/ (reduce + (map #(+ (* % %) (* %2 %2)) sds means)) n)
                       (* new-mean new-mean)))}))

(defn gen [mean sd]
  (let [r (java.util.Random.)]
    (repeatedly (fn [] (+ mean (* sd (.nextGaussian r)))))))

(comment
  (let [n 1000
        vs1 (take n (gen 1 1))
        vs2 (take n (gen 20 10))
        vs3 (take n (gen 5 5))
        all (concat vs1 vs2 vs3)
        stats (fn [vs] {:mean (timeseries.functions/mean vs) :min (reduce min vs) :max (reduce max vs) :sd (timeseries.functions/sd vs)})]
    {:real {:mean (timeseries.functions/mean all)
            :sd (timeseries.functions/sd all)}
     :aggregated (aggregation-of (stats vs1) (stats vs2) (stats vs3))})
  )


