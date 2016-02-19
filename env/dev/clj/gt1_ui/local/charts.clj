(ns gt1-ui.local.charts
  (:require [incanter.charts :as ch]
            [incanter.core :refer [view]] 
            [chart-utils.jfreechart :as cjf]
            [clj-time.coerce :as tc]))

(defn- adjust-legend-font [^org.jfree.chart.ChartFrame chartFrame n] 
  (let [legend (.. chartFrame getChartPanel getChart getLegend)
        font (.getItemFont legend)
        font-size (.getSize font)
        target-font-size (condp > n
                           20 font-size
                           30 (dec font-size)
                           40 (- font-size 2)
                           60 (- font-size 4)
                           5)]
    (.setItemFont legend (.deriveFont font (float target-font-size)))))

(defn plot-parsed-data 
  "Plot a map of series name to number sequence on an interactive line chart.
If `ts` might be unixtimestamps, the x axis will be a date-time axis.
"
  [ts datas {:keys [title]}]
  (let [ts (mapv tc/to-long ts)
        names (keys datas)
        plot-fn (if (zero? (first ts)) ch/xy-plot* ch/time-series-plot*)
        chart (plot-fn ts (into [] (get datas (first names))) 
                       :series-label (first names)
                       :legend true
                       :title title)]
    (doseq [name (rest names)]
           (ch/add-lines chart ts (into [] (get datas name)) :series-label name)) 
    (cjf/set-color-brewer-colors chart)
    (cjf/set-discontinuous chart)
    (doto (view chart)
      (-> .getChartPanel cjf/add-mouse-listener)
      (adjust-legend-font (count names)))
    chart))
