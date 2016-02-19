(ns gt1-ui.data.availability
  (:require [clj-time
             [coerce :as c]
             [core :as time]
             [periodic :as p]]
            [gt1-ui.util :as util]
            [gt1-ui.data.db :as db]
            [org.clojars.smee.map :refer [map-values]]))

(defn- date-only [dt]
  (let [dt (c/to-date-time dt)]
    (time/date-time (time/year dt) (time/month dt) (time/day dt))))

(defn- plant-availability-per-day
"Return all operation mode changes in a time interval for a plant."
  [events]
  (when (not-empty events)
    (let [ONE-DAY (time/days 1)
          s (:timestamp (first events))
          e (:timestamp (last events))
          m (reduce (fn [[since old m] {:keys [timestamp new-mode] :as event}]
                      (let [now timestamp
                            today (date-only now)
                            yesterday (date-only since)
                            ;; if we passed midnight, add remaining seconds of the last day to the currently active status
                            m (if (time/before? yesterday today)
                                (update-in m [yesterday old] (fnil + 0) (time/in-seconds (time/interval since (time/plus yesterday ONE-DAY))))
                                m)
                            ;; set last known status for intermediary days without status changes
                            m (reduce (fn [m day] (assoc-in m [day old] (* 24 60 60))) 
                                     m
                                     (p/periodic-seq (time/plus yesterday ONE-DAY) today ONE-DAY))]
                        [(time/latest since now) ; time/date of the last state change
                         new-mode ; new state
                         (update-in m [today old] ;calculate duration of this event in today
                                    (fnil + 0)
                                    (time/in-seconds (time/interval (time/latest since today) now)))]))
                    [s (:new-mode (first events)) {}]                  
                    events)]
      (last m))))

(defonce avail-cache (atom nil))
(defonce avail-agg-cache (atom nil))

(defn fill-avail-cache! [conn start-date end-date]
  (reset! avail-cache (db/plant-mode-changes conn start-date end-date))
  (count (reset! avail-agg-cache (map-values (fn [events] (plant-availability-per-day events)) @avail-cache))))

(defn modes [conn]
  @avail-cache)

(defn aggregated-modes [conn]
  @avail-agg-cache)

(defn detailed-modes
"List of timestamp/end/mode maps for each individual mode time interval in `from` till `to`."
  [conn plant from to]
  (let [switches (get (modes conn) plant)
        [before switches] (split-with #(time/before? (:timestamp %) from) switches)
        before (assoc (last before) :timestamp from)
        [within [after]] (split-with #(time/after? to (:timestamp %)) switches)
        after (assoc after :timestamp to)
        switches (conj (into [before] within) after)]
    (->> switches
         (keep identity)
         (partition 2 1)
         (mapv (fn [[a b]] {:timestamp (:timestamp a)
                           :end (:timestamp b)
                           :mode (:new-mode a)})))))



(defn operation-time-intervals [conn plant start end]
  (->> (detailed-modes conn plant start end)
       (util/completely-within-mode 7)))
