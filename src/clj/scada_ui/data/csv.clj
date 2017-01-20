(ns scada-ui.data.csv
  (:require 
            [clj-time
             [core :as t]
             [format :as tf]
             [coerce :as tc]
             [periodic :as tp]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.clojars.smee
             [map :refer [map-values]]])
  (:import java.text.NumberFormat
           java.text.SimpleDateFormat
           java.util.Locale
           java.util.TimeZone
           java.util.Calendar))


(defn dates-seq 
  "Create sequence of 10min date-times starting at start-date."
  [start-date end-date] 
  (tp/periodic-seq (tc/to-date-time start-date) (tc/to-date-time end-date) (t/minutes 10)))

(defn parse-csv [rdr]
  (->> rdr line-seq (map #(.split ^String % ";"))))

(defn parse-10mwk-file 
  "Each csv file has the following columns (no headers): 
'10mwk';component id;sensor id;[avg,max,min,sd]{144}
Instead, return a map of series name to sequence of values. Additionally, add column :timestamp."
  [in start-date]
  (let [end-date (t/plus (tc/to-date-time start-date) (t/days 1))]
    (try
      (with-open [rdr (io/reader in)]
        (let [rows (parse-csv rdr)]
         (reduce (fn [m [_ i1 i2 & tuples]]
                   (let [tuples (map #(.split ^String % ",") tuples)
                         [avgs maxs mins sds] (apply map vector tuples)]
                     (assoc m
                            [(str i1 "/" i2) :avg] avgs
                            [(str i1 "/" i2) :std] sds
                            [(str i1 "/" i2) :min] mins
                            [(str i1 "/" i2) :max] maxs)))

                 {"timestamp" (dates-seq start-date end-date)}
                 rows)))
      (catch Exception e
        (.printStackTrace e)
        {}))))

(defn write-csv
  "Appends all rows, columns separated by ';' into file `out`."
  [out rows]
  (with-open [bw (io/writer out :append true)]
    (doseq [row rows]
      (.write bw (str/join ";" row))
      (.write bw "\n"))))

(comment
  (parse-structure "e:/datasets/gt1/M5000-de.xml")

  (require 'clojure.inspector)
  (clojure.inspector/inspect-tree (parse-structure "e:/datasets/gt1/M5000-de.xml"))
  
  )
