(ns gt1-ui.util
  (:require #?(:cljs [cljs-time.core :as time]
               :clj [clj-time.core :as time])))

(defn kw-name
"The keyword may contain slashes. Because clojure interpretes these as namespaces, `name` will drop the namespace"
  [kw]
  #?(:clj (str (.-sym ^clojure.lang.Keyword kw))
     :cljs (name kw)))

(defn- without-seconds [ts] ;; can't use time/floor, does not exist in cljs-time
  (time/date-time (time/year ts) (time/month ts) (time/day ts) (time/hour ts) (time/minute ts)))

(defn completely-within-mode
"All our measurements represent 10min intervals. Now, in some contexts we might be only interested
in time intervals that completely comprise such full 10min intervals. This function
adjusts ..."
  [mode modes]
  (->> modes
       (filter #(= mode (:mode %)))
       (map (fn [{ts :timestamp e :end}]
          ;; make sure we remove partial time intervals
          ;; we are only interested in full 10min interval measurements.
          ;; so if any interval is overlapping the 10min boundary we skip forward/backward to the next/last 10min boundary
          (let [width (* 10 60) ;; 10min intervals per measurement
                s-minutes (mod (+ (time/second ts) (* 60 (time/minute ts))) width)
                e-minutes (mod (+ (time/second e) (* 60 (time/minute e))) width)
                ts (if (zero? s-minutes) ts (time/plus ts (time/seconds (- width s-minutes))))
                e (if (zero? e-minutes) e (time/minus e (time/seconds e-minutes)))]
            [(without-seconds ts) (without-seconds e)])))
       (filter (fn [[a b]] (time/before? a b)))))
