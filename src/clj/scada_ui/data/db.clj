(ns scada-ui.data.db
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.jdbc :as sql] 
            [org.clojars.smee.map :refer [map-values]]
            [org.clojars.smee.seq :refer [distinct-by]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clj-time.periodic :as tp]
            [scada-ui.util :refer [kw-name merge-events]]
            [taoensso.nippy :as nippy]
            [clj-time.core :as time]
            [scada-ui.data.structure :as struc]
            [clojure.set :as set])
  (:import com.mchange.v2.c3p0.ComboPooledDataSource)
  (:import [java.sql PreparedStatement]
           java.util.Locale))

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
  (let [s (str s)]
    (if (re-matches #".*[`'\"%\\].*" s)
      (throw (ex-info "Possible SQL injection via MySQL entity!" {:token s}))
      (str "`" (str/trim s) "`"))))

;;;;;;;;;;;;;;; connection pools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce ^:private connection-pools (atom {}))

(defn- create-db-connection-pool
  [{:keys [classname connection-name parameters connection-uri]}]
  (let [{:keys [userInfo host scheme port query path]} (bean (java.net.URI. connection-uri))
        [user password] (str/split userInfo #":")
        url (format "jdbc:mysql://%s:%d%s" host port path)
        cpds (doto (ComboPooledDataSource. (or connection-name path))
               (.setDriverClass classname)
               (.setJdbcUrl url)
               (.setProperties (reduce (fn [p [k v]] (.setProperty p (str k) (str v)) p) (java.util.Properties.) parameters))
               (.setUser user)
               (.setPassword password)
               (.setMaxPoolSize 100)
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
                "defaultFetchSize" 1000
                "useLegacyDatetimeCode" false
                "noTimezoneConversionForDateType" true
                "serverTimezone" "UTC"}})

(defn create-connection-pool! [db-name spec]
  (let [conn (create-db-connection-pool spec)]
    (swap! connection-pools assoc db-name (with-meta conn {:db-spec spec}))
    conn))

(defn connection [db-name]
  (get @connection-pools db-name))

;;;;;;;;;;; nippy binary compact data structure representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deserialize [key]
  (fn [m]
    (update m key nippy/thaw)))

(defn- serialize [clj-obj]
  (nippy/freeze clj-obj))

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
                      [:day_of_month "tinyint"]
                      [:hour_of_day "tinyint"]])

(defn create-series-table-command [name headers]
  (mysql-table
   name
   (into date-dimensions
         (for [hdr headers]
           [hdr "double"]))))

(def dateformat-full (tf/formatter "yyyy-MM-dd HH:mm:ss.SSS"))
(def dateformat-date (tf/formatter "yyyy-MM-dd"))

(defn create-datetime-entries [date]
  [(tf/unparse dateformat-full date) ; sql timestamp
   (tf/unparse dateformat-date date) ; sql timestamp
   (tc/to-long date) ;unix timestamp
   (time/year date)
   (time/month date)
   (time/day date)
   (time/hour date)])

(defn create-tables! [conn]
  (sql/db-do-commands conn
     [(mysql-table :imported
                   [[:filename "varchar(2048)"]
                    [:table "varchar(127)" "comment 'lookup'"]
                    [:plant "varchar(127)" "comment 'lookup'"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :events ;; explicit representation of error intervals
                   (into date-dimensions
                         [[:end "datetime" "default 0"]
                          [:end-unixtimestamp "bigint"]
                          [:plant "varchar(127)" "comment 'lookup'"]
                          [:user "varchar(127)" "comment 'lookup'"]                          
                          [:type "varchar(127)" "comment 'lookup'"]
                          [:id "varchar(127)" "comment 'lookup'"]]))
      (mysql-table :parameter-changes
                   (into date-dimensions
                         [[:plant "varchar(127)" "comment 'lookup'"]
                          [:user "varchar(100)" "comment 'lookup'"]
                          [:id "varchar(127)" "comment 'lookup'"]
                          [:new "double"] ; mapping TRUE to 1 and FALSE to 0
                          [:old "double"]]))
      (mysql-table :plants
                   [[:name "varchar(127)"]
                    [:type "varchar(127)"]
                    [:latitude "double"]
                    [:longitude "double"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :plant-specs
                   [[:name "varchar(127)"]
                    [:data "longblob"]]
                   {:table-spec "engine = 'MyIsam'"})
      ;; stateful tables, may change contents
      (mysql-table :algorithms
                   [[:id "integer" "primary key" "auto_increment"]
                    [:plant "varchar(127)"]                   
                    [:learn-start "datetime"]
                    [:learn-end "datetime"]                    
                    [:algorithm "longblob"]
                    [:active "boolean"]
                    [:options "longblob"]
                    [:type "varchar(45)"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :algorithm-states
                   [[:algorithm "integer"]
                    [:plant "varchar(127)"]                   
                    [:last-update "datetime"]
                    [:state "longblob"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :algorithm-results
                   [[:algorithm "integer"]
                    [:timestamp "datetime"]
                    [:results "longblob"]]
                   {:table-spec "engine = 'MyIsam'"})
      (mysql-table :acknowledgment
                   [[:plant "varchar(127)"]
                    [:sensor "varchar(127)"]
                    [:editted "datetime"]
                    [:start "datetime"]
                    [:end "datetime"]
                    [:reason "varchar(127)"]
                    [:notes "longblob"]]
                   {:table-spec "engine = 'MyIsam'"})]))

#_(defn clear-db! [conn]
  (doseq [table (mapcat vals (sql/query conn "show tables"))]
    (sql/db-do-commands conn [(str "drop table " (entities table))])))

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

(defn insert-plant-types! [conn plant-specs]
  (sql/insert-multi! conn :plant-specs (for [{n :name :as pt} plant-specs] {:name n :data (serialize pt)}) {:entities entities}))

;;;;;;;;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adhoc "Adhoc query, for development" [conn query & params]
  (sql/query conn (apply vector query params)))

(defn do! "Run adhoc commands, like drop, create table, etc." [conn cmds]
  (sql/db-do-commands conn cmds))

(defn imported? [conn table filename]
  (= 1 (:count (first (sql/query conn ["select count(*) as count from `imported` where `table`=? and `filename`=?" table (str filename)])))))

;;;;;;;; plants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn plants [conn]
  (sql/query conn ["select * from plants order by name"] {:result-set-fn vec}))

(defn plant [conn plant]
  (sql/query conn ["select * from plants where name=?" plant] {:result-set-fn first}))

(defn plant-specs [conn]
  (sql/query conn ["select data from `plant-specs`"] {:row-fn (comp :data (deserialize :data)) :result-set-fn #(->> % (group-by :name) (map-values first))}))

(defn plant-spec [conn name]
  (sql/query conn ["select data from `plant-specs` where name=?" name] {:row-fn (deserialize :data) :result-set-fn (comp :data first)}))

(defn plant-spec-names [conn]
  (sql/query conn ["select name from `plant-specs"] {:row-fn :name :result-set-fn set}))

(defn plant-spec-of [conn plant]
  (sql/query conn ["select `data` from `plant-specs` where `name`=(select `type` from `plants` where `name`=?)" plant] {:row-fn (deserialize :data) :result-set-fn (comp :data first)}))

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

(defn- series-parts
"Series may be a concattenation of individual series."
  [s]
  (str/split s #"\+"))


(defn- by-series
"Creates a function that can transform a sequence of database result rows into a map {{:keys [series name]} [chartdata]}. Can reconstruct sums of sensors."
  [series]
  (let [kw (memoize (fn [s suffix] (keyword (str s suffix))))
        split (memoize series-parts)
        by-name (group-by :plant series)
        extract-value (fn [series row]
                        (let [v-min (row (kw series "/min"))
                              v-max (row (kw series "/max"))
                              v-avg (row (kw series "/avg"))]
                          [(or v-min v-avg) v-avg (or v-max v-avg)]))
        extract-values (fn [series row]
                         (->> series
                              split
                              (map #(extract-value % row))
                              (reduce (partial mapv +))))]
    (fn [rows]
      (reduce (fn [m {p :plant ts :timestamp :as row}]
                (reduce (fn [m {s :series :as spec}]
                          (let [vs (extract-values s row)]
                            (update m spec (fnil conj []) [ts vs])))
                        m (by-name p)))
              {} rows))))

(defn rolled-up-values
  "Find min, max, and average of values aggregated into `num` time slots.
Order: `(for [table tables, col columns] (str table col))`"
  [conn series start-time end-time num only-production?]
  (if (empty? series)
    {}
    (let [s (tc/to-long start-time) 
          e (tc/to-long end-time)
          num (max 1 num)
          interval-in-s (max 1 (long (/ (- e s) num)))
          queries (for [[plant groups] (group-by :plant series)
                        :let [prod-col (struc/production-column plant)
                              prod (if only-production? (str " and " (entities prod-col) " = 1.0") "")
                              columns (for [{p :plant s :series} groups, s (series-parts s)
                                            :let [point-type (struc/sensor-type p s)]]
                                        ;; use plant-spec to determine whether to use dedicated min/max columns or min/max of the time interval group
                                        (format "avg(`%1$s/avg`) as `%1$s/avg`, min(`%2$s`) as `%1$s/min`, max(`%3$s`) as `%1$s/max`"
                                                s
                                                (if (point-type :min) (str s "/min") (str s "/avg"))
                                                (if (point-type :max) (str s "/max") (str s "/avg"))))
                              table (entities plant)]]
                    (format "(select '%s' as plant, unixtimestamp as timestamp, %s from %s where timestamp >= ? and timestamp < ? %s group by (unixtimestamp div ?))" plant (str/join "," columns) table prod))
          query (format "select * from (%s) as combined order by combined.plant, combined.timestamp" (str/join " UNION ALL " queries))]
      (if (zero? (count queries))
        {}
        (sql/query conn (apply vector query (take (* 3 (count queries)) (cycle [start-time end-time interval-in-s])))
                   {:row-fn #(update % :timestamp tc/from-long)
                    :result-set-fn (by-series series)})))))

(defn traces
  "Query full trace data. Needs to query `unixtimestamp` fields because in Infobright
DATETIME does not support milliseconds."
  [conn series start-time end-time]
  (let [queries (for [[plant groups] (group-by :plant series)
                      :let [columns (for [{p :plant s :series} groups
                                          ;; some series do not have associated traces, need to filter columns before fetching data!
                                          :let [param (struc/find-trace-name (:name (struc/plant-spec-of p)) s)]
                                          :when param]
                                      (str (entities param) " as " (entities (str s "/avg"))))
                            table (entities (str plant "-traces"))]
                      :when (not-empty columns)]
                  (format "(select '%s' as plant, unixtimestamp as timestamp, %s from %s where timestamp >= ? and timestamp < ?)" plant (str/join "," columns) table))
        query (format "select * from (%s) as combined order by combined.timestamp" (str/join " UNION ALL " queries))] 
    (if (zero? (count queries))
      {}
      (sql/query conn (apply vector query (take (* 2 (count queries)) (cycle [start-time end-time])))
                 {:row-fn (fn [m] (update m :timestamp tc/to-date-time))
                  :result-set-fn (by-series series)}))))

;;;;;;;;;;;;;; functions to process many/all rows in a table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn process-raw-values-in-time-range
  "Applies a function `func` to each result row. Each row's key is a string, not a keyword.
This function can be used to stream multiple sensor measurements per timestamp without 
loading all data into memory."
  [conn table columns start end production-only? func]
  {:pre [(not-empty columns)]}
  (let [prod-col (struc/production-column table)
        params (map entities columns)
        prod-q (if production-only? (str " and "(entities prod-col) "=1") "")
        query (format "select timestamp, %s from %s where (timestamp >= ? and timestamp <= ? %s) order by timestamp" (str/join "," params) (entities table) prod-q)]
    (sql/query conn [query start end]
               {:row-fn #(map-values kw-name identity %)
                :result-set-fn func
                :result-type :forward-only
                :fetch-size 10
                :concurrency :read-only})))


(defn raw-value-in-time-range
  "All values for each column, returns map of column to values in time order."
  [conn table columns start end production-only?]
  {:pre [(every? string? columns)
         (not-empty columns)]}
  (process-raw-values-in-time-range conn table columns start end production-only?
     (fn [rows]
       (let [cols (into {"timestamp" []} (map vector columns (repeat (vector-of :double))))]
         (reduce (fn [cs row]
                   (reduce (fn [cs c] (update cs c conj (get row c)))
                           cs (keys row)))
                 cols rows)))))

;;;;;;;;;;;;;;; events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn errors
  "Find error intervals for power plants. Returns `{plant [error1,...]}`. 
   Needs to query `unixtimestamp` fields because in Infobright DATETIME does not support milliseconds"
  [conn plants start-time end-time]
  (let [subquery (str/join " or "(repeat (count plants) "plant = ?"))
        s start-time
        e end-time]
    (sql/query conn (apply vector (str "select unixtimestamp as 'timestamp',`end-unixtimestamp` as end,user,plant,id,'error' as `type` from events where type='error' and ((timestamp >= ? and timestamp < ?) or (end > ? and end <= ?)) and (" subquery ") order by timestamp") s e s e plants)
               {:row-fn #(-> % (update :timestamp tc/from-long) (update :end tc/from-long))
                :result-set-fn (fn [rows]
                                 (->> rows
                                      (group-by :plant)
                                      (map-values merge-events)
                                      vals
                                      (reduce into [])))})))

(defn modes
  "Find mode intervals for power plants. Returns `{plant [mode,...]}`
  Needs to query `unixtimestamp` fields because in Infobright DATETIME does not support milliseconds"
  [conn plants start-time end-time]
  (let [subquery (str/join " or "(repeat (count plants) "plant = ?"))
        s start-time
        e end-time]
    (sql/query conn (apply vector (str "select unixtimestamp as 'timestamp',`end-unixtimestamp` as `end`,user,plant,id,'status' as `type` from events where type='status' and ((timestamp >= ? and timestamp < ?) or (end > ? and end <= ?)) and (" subquery ") order by timestamp") s e s e plants)
               {:row-fn #(-> % (update :timestamp tc/from-long) (update :end tc/from-long))
                :result-set-fn (fn [rows]
                                 (->> rows
                                      (group-by :plant)
                                      (map-values merge-events)
                                      vals
                                      (reduce into [])))})))

(defn aggregated-modes [conn plants start-time end-time] ;; FIXME only works if events are not longer than one day (start end end need to be within the same day)
  (let [subquery (str/join " or "(repeat (count plants) "plant = ?"))]
    ;; query is structure to avoid fetching status events where start is the same as end (happens if a status change occured on midnight, taking less than one second)
    (sql/query conn (apply vector (str "SELECT plant, day(date) as day, month(date) as month, year(date) as year, sum(`end-unixtimestamp`-`unixtimestamp`) as duration, id as 'mode' "
                                       "FROM `events` "
                                       "where `type`='status' and `timestamp` >= ? and `timestamp` < ? and `end` <= ? "
                                       "and (" subquery ") "
                                       "group by plant,date,id order by plant,date") start-time end-time end-time plants)
               {:row-fn (fn [{:keys [day month year] :as row}] (-> row (dissoc :day :month :year) (assoc :date (time/date-time year month day))))
                :result-set-fn (fn [rows]
                                 (let [by-date (reduce (fn [res {:keys [plant date mode duration]}]
                                                         (assoc-in res [date plant mode] (long duration)))
                                                       {} rows)
                                       plants (set plants)]
                                   (reduce-kv (fn [m date by-plant]
                                                (let [existing (set (keys by-plant))
                                                      missing (set/difference plants existing)
                                                      by-plant' (reduce (fn [m p] (assoc m p {"unknown" (* 24 60 60 1000)})) by-plant missing)]
                                                  (reduce-kv (fn [m p vs] (assoc-in m [p date] vs)) m by-plant')))
                                              {} by-date)))})))

(defn plant-modes-stats
"Transform results of `db/aggregated-modes` to relative frequencies per plant/day."
  [agg-modes]
  (let [div (double (* 24 60 60 1000))]
    (map-values (partial map-values (partial map-values #(/ % div))) agg-modes)))

(defn park-modes-stats
"Create `{date {status relative-frequency-over-park}}` from the results of `db/aggregated-modes`."
  [agg-modes]
  (let [div (double (* (count agg-modes) 24 60 60 1000))
        by-date (apply merge-with (partial merge-with +) (vals agg-modes))]
    (map-values (partial map-values #(/ % div)) by-date)))

(defn parameter-changes
  [conn plants start-time end-time]
  (let [subquery (str/join " or "(repeat (count plants) "plant = ?"))
        s start-time
        e end-time]
    (sql/query conn (apply vector (str "select timestamp,user,plant,id,`new`,`old` from `parameter-changes` where (timestamp >= ? and timestamp < ?) and (" subquery ") order by timestamp") s e plants)
               {:result-set-fn vec})))

;;;;;;;;;;;;;; anomaly analysis CRUD ;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; algorithms/models ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn plants-with-algorithms
  ([conn] (plants-with-algorithms conn nil))
  ([conn type]   
   (sql/query conn (if type
                     ["select distinct(plant) from algorithms where active=true and `type`=? order by plant" type]
                     ["select distinct(plant) from algorithms where active=true order by plant"])
              {:result-set-fn vec})))

(defn algorithms
  ([conn]
   (sql/query conn ["select * from algorithms where active=true"] {:row-fn (comp (deserialize :options) (deserialize :algorithm)) :result-set-fn vec}))
  ([conn plant]
   (sql/query conn ["select * from algorithms where active=true and plant=?" plant] {:row-fn (comp (deserialize :options) (deserialize :algorithm)) :result-set-fn vec})))

(defn algorithms-by [conn {:keys [plant type]}]
  (sql/query conn
             (cond
               (and plant type) ["select * from algorithms where plant=? and type=?" plant type]
               plant ["select * from algorithms where plant=?" plant]
               type ["select * from algorithms where type=?" type])
             {:row-fn (comp (deserialize :options) (deserialize :algorithm)) :result-set-fn vec}))

(defn algorithm-by-id [conn id]
  (sql/query conn ["select * from algorithms where id=?" id]
             {:row-fn (comp (deserialize :options) (deserialize :algorithm)) :result-set-fn first}))

(defn store-algorithm!
"Serialize model, store in DB, return generated ID."
  [conn plant from to model type options]
  (let [text (serialize model)
        opts (serialize options)]
    (:generated_key (first (sql/insert! conn :algorithms {:plant plant
                                                          :algorithm text
                                                          :options opts
                                                          :learn-start from
                                                          :active true
                                                          :learn-end to
                                                          :type type}
                                        {:entities entities})))))


(defn update-algorithm! [conn id algorithm]
  (sql/update! conn :algorithms {:algorithm (serialize algorithm)} ["id=?" id] {:entities entities}))

;;;;;;;;;;;; persistent algorithm processing state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn algorithm-state [conn algorithm-id]
  (sql/query conn ["select state,`last-update` from `algorithm-states` where algorithm=?" algorithm-id] {:row-fn (deserialize :state) :result-set-fn first}))

(defn store-algorithm-state! [conn plant algorithm-id last-processed-timestamp state]
  (if (zero? (sql/query conn ["select count(*) as c from `algorithm-states` where `algorithm`=? and plant=?" algorithm-id plant] {:row-fn :c :result-set-fn first}))
    (sql/insert! conn :algorithm-states {:algorithm algorithm-id :plant plant :last-update last-processed-timestamp :state (serialize state)} {:entities entities})
    (sql/update! conn :algorithm-states {:last-update last-processed-timestamp :state (serialize state)} ["algorithm=? and plant=?" algorithm-id plant] {:entities entities})))

(defn delete-algorithm-state! [conn id]
  (sql/delete! conn :algorithm-states ["algorithm=?" id] {:entities entities}))

(defn delete-algorithm-results! [conn id]
  (sql/delete! conn :algorithm-results ["algorithm=?" id] {:entities entities}))
;;;;;;;;;;; daily algorithm results for users ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn algorithm-results [conn ids from to]
  {:pre [(not-empty ids)]}
  (let [ids-query (str/join "," (repeat (count ids) "?"))
        query (format "select x.algorithm, y.plant, x.timestamp, x.results from `algorithm-results` x inner join `algorithms` y on x.algorithm=y.id where timestamp>=? and timestamp<? and x.algorithm in (%s) order by timestamp ASC" ids-query)]
    (sql/query conn (apply vector query from to ids)
                     {:row-fn (deserialize :results)
                      :result-set-fn vec})))

(defn recent-algorithm-ids
"Find most recent ids of all active algorithms per plant and algorithm type."
  [conn]
  (sql/query conn "SELECT t1.id FROM algorithms t1
  JOIN (SELECT plant, MAX(`learn-end`) timestamp FROM algorithms GROUP BY plant,`type`) t2
    ON t1.plant = t2.plant AND t1.`learn-end` = t2.timestamp"  {:row-fn :id :result-set-fn vec}))

(defn algorithm-ids
"Find all stored algorithm ids for a specific algorithm type."
  [conn type]
  (sql/query conn ["select id from algorithms where `type`=?" type] {:row-fn :id :result-set-fn vec}))

(defn algorithms-per-plant [conn]
  (sql/query conn ["select plant, id, `learn-start`, `learn-end`, type, active as 'active?' from algorithms"] {:result-set-fn vec}))

(defn store-algorithm-result! [conn algorithm-id date results] ;; FIXME there are duplicated results for same id+date?
  (sql/insert! conn :algorithm-results {:results (serialize results) :timestamp date :algorithm algorithm-id} {:entities entities}))


;;;;;;;;;;;;;;;;;;; specific queries ;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch-wind-stats [conn plants sector-angle from to]
  {:pre [(number? sector-angle)
         (<= 1 sector-angle 360)
         (zero? (mod 360 sector-angle))
         (every? entities plants)]}
  ;; relative wind direction 14/13
  ;; nacelle direction 13/10
  ;; wind speed 1+2 14/12  
  (let [|sectors| (/ 360 sector-angle)
        qs (for [plant plants ;; aquire the sensor names for wind stats, each plant may have different data (GT: relative wind direction+nacelle direction, other plants: direct wind direction)
                 :let [{:keys [speed direction]} (-> plant struc/plant-spec-of :functions :wind)
                       direction (str/join "+ "(map #(entities (str % "/avg")) (if (not (vector? direction)) [direction] direction)))
                       speed (entities (str speed "/avg"))]]
             (format (str "select '%1$s' as plant, "
                          "mod(mod(mod(%4$s,360)+360,360) div %2$d,%3$d) as 'dir', "
                          "avg(%5$s) as 'speed', "
                          "count(*) as n "
                          "from `%1$s` "
                          "where timestamp >= ? and timestamp < ? group by dir")
                     plant (int sector-angle) (int |sectors|) direction speed))
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


(defn series->column
  ([s] (series->column s "/avg"))
  ([s suffix] (series->column s suffix ""))
  ([s suffix prefix]
   (str/join "+" (map #(str (when (not-empty prefix) (str prefix "."))
                            (entities (str % suffix)))
                      (series-parts s)))))

(defn query-extents
"Find minima and maxima for two sensor columns. Returns map with keys `[:min-x :max-x :min-y :max-y]`"
  [conn plant column from to only-production?]
  (let [prod-col-q (if only-production? (format " and %s=1" (entities (struc/production-column plant))) "")
        query (format "select min(%1$s) as 'min', max(%1$s) as 'max' from %2$s where timestamp between ? and ? %3$s" column (entities plant) prod-col-q)]
    (sql/query conn [query from to] {:result-set-fn (fnil first {:min 0 :max 1})})))


(defn heatmap-of
"Create data for a heatmap of two columns. Aggregates into `x-step`x`y-steps` bins, the extent
of the data is estimated from the data in the time interval queried. Returns a vector of rows of counts as well as the 
extents."
  [conn plant-x name-x plant-y name-y from to only-production? {:keys [x-steps y-steps min-x max-x min-y max-y] :or {x-steps 10 y-steps 10}}]
  (let [;name-y (series->column name-x "/std" "t2")
        name-x (series->column name-x "/avg" "t1")
        name-y (series->column name-y "/avg" "t2")        
        x-extent (- max-x min-x)
        y-extent (- max-y min-y)        
        ;; per default the current system's locale gets used to render decimal separators. MySQL will then interprete a comma as a argument separator, which fails. Enforce US locale to make sure that the decimal is a dot
        x-q (String/format Locale/US "floor(%d * (c.a - %f)/%f)" (to-array [x-steps (double min-x) (if (zero? x-extent) 1.0 (double x-extent))]))
        y-q (String/format Locale/US "floor(%d * (c.b - %f)/%f)" (to-array [y-steps (double min-y) (if (zero? y-extent) 1.0 (double y-extent))]))
        prod-col-q (if only-production? (format " and t1.%s=1 and t2.%s=1" (entities (struc/production-column plant-x)) (entities (struc/production-column plant-y))) "")
        join (format "select %s as a, %s as b from %s t1 inner join %s t2 on t1.timestamp=t2.timestamp where t1.timestamp between ? and ? %s" name-x name-y (entities plant-x) (entities plant-y) prod-col-q)
        query (format "select %s as x, %s as y, count(*) as n from (%s) c group by x,y" x-q y-q join)]
    (sql/query conn [query from to]
               {:result-set-fn (fn [rows]
                                 (let [data (vec (repeat y-steps (vec (repeat x-steps 0))))]
                                   {:extents {:min-x min-x :max-x max-x :min-y min-y :max-y max-y}
                                    :data (reduce (fn [data {:keys [x y n]}]
                                                    (update-in data [(max 0 (min (dec y-steps) (int y)))
                                                                     (max 0 (min (dec x-steps) (int x)))] + n))
                                                  data rows)}))})))

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






(comment

  (let [conn scada-ui.handler/conn]
    (doseq [id (algorithm-ids conn) :let [opts (:options (:algorithm (algorithm-by-id conn id)))]]
      (println id opts)))
  
  (defn delete-after! [conn since]
    (let [e {:entities entities}]
      ;; delete imported for each date from date till today
      [(doall (for [d (tp/periodic-seq since (time/with-time-at-start-of-day (time/now)) (time/days 1))]
                (sql/delete! conn :imported [(format "filename like '%%%s%%'" (tf/unparse dateformat-date d))] e)))
       (sql/delete! conn :parameter-changes ["timestamp >= ?" since] e)
       (sql/delete! conn :events ["timestamp >= ? or end >= ?" since since] e)
       (doall
        (for [p (map :name (plants conn))]
          [(sql/delete! conn p ["timestamp >= ?" since] e)
           (sql/delete! conn (str p "-traces") ["timestamp >= ?" since] e)]))]))

  (delete-after! scada-ui.handler/conn (time/date-time 2016 12 8))
  
  (defn delete-plant-tables! [conn]
    (doseq [p (sort (map :name (plants conn))) :let [tbl p #_(str p "-traces")]]
      (do
        (println tbl)        
        #_(time (do! conn (str "drop table " (entities tbl))))
        #_(sql/delete! conn :imported [(str "`table`='" tbl "'")]))))
  
  (delete-plant-tables! scada-ui.handler/conn)
  (do! scada-ui.handler/conn "delete from `plant-specs`")
  ;; transform results to explicit `since`
  (time
   (let [c scada-ui.handler/conn
         all-results (algorithm-results c (algorithm-ids c "lasso") (time/date-time 2000) (time/now))]
     (do! c "delete from `algorithm-results`")
     (doseq [{ts :timestamp
              id :algorithm
              {:keys [faults candidates]} :results}
             all-results
             :let [faults' (map-values :n faults)
                   since (map-values :since faults)
                   res' {:faults faults' :since since :candidates candidates}]]
       (store-algorithm-result! c id ts res'))))
  )
