(ns scada-ui.util
  (:require #?(:cljs [cljs-time.core :as time]
               :clj [clj-time.core :as time]))
  (:import #?@(:clj [[org.apache.commons.codec.digest DigestUtils]]
               :cljs [[goog.crypt Md5]])))

(defn md5
  "Compute the hex MD5 sum of a byte array."
  [str]
  #?(:clj
     (DigestUtils/md5Hex ^String str)
     :cljs (let [hash (.digest (doto (goog.crypt.Md5.) (.update str)))]
             (goog.crypt.byteArrayToHex hash))))

(defn kw-name
"The keyword may contain slashes. Because clojure interpretes these as namespaces, `name` will drop the namespace"
  [kw]
  #?(:clj (str (.-sym ^clojure.lang.Keyword kw))
     :cljs (name kw)))

(defn map-values 
  "Change all values or all keys and values by applying a function to each of them."
  ([vf m] (map-values identity vf m))
  ([kf vf m]
   (reduce-kv (fn [m k v] (assoc m (kf k) (vf v))) (empty m) m)))

(defn approximately-adjacent
"on day changes the first timestamp might be off by a few milliseconds"
  [ts1 ts2]
  (or (= ts1 ts2)
      (and (time/after? ts2 ts1)
           (< (time/in-seconds (time/interval ts1 ts2)) 1))))

(defn merge-events
  "combine adjacent events with same id, artifacts from our daily imports (each event starts/ends max. at midnight)"
  [all-events]
  (if (empty? all-events)
    all-events
    (loop [events (next all-events), previous (first all-events), res []]
      (if (nil? events)
        (conj res previous)
        (let [{ts1 :timestamp end1 :end id1 :id} previous
              [{ts2 :timestamp end2 :end id2 :id :as event} & events] events]
          (if (and (= id1 id2)
                   end1 end2
                   (or (approximately-adjacent end1 ts2)
                       ;overlapping due to zoom
                       (time/overlaps? ts1 end1 ts2 end2)))
            (recur events (assoc previous :end end2) res)
            (recur events event (conj res previous))))))))

(defn time-interval-differences
"Return sequence of `time/interval`s that resemble the difference of the time intervals 
`int1` and `int2`. That is: 0, 1 or two time intervals that are in `int1` but not in `int2`"
  [int1 int2]
  (let [common (time/overlap int1 int2)]
    (if (nil? common)
      [int2]
      (let [diff1 (time/interval (time/start int2) (time/start common))
            diff2 (time/interval (time/end common) (time/end int2))]
        (remove #(zero? (time/in-millis %)) [diff1 diff2])))))
