(ns gt1-ui.calendar
  (:require cljsjs.d3
            [reagent.core :as r]
            [reagent.format :refer [format]]
            [reagent.interop :refer-macros [$]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            cljs-time.extend
            [cljs-time.format :as tf]
            [cljs-time.periodic :as tp]
            [gt1-ui.pages.common :refer [human-date parse-human-date]]
            [clojure.string :as str]))
;; bug fix: cljs-time.extend cares about IEquiv and IComparable, but ignores IHash which we need to be able to find date-times in maps
(extend-type goog.date.UtcDateTime IHash (-hash [this] (tc/to-long this)))
(extend-type goog.date.Date IHash (-hash [this] (tc/to-long this)))
(extend-type goog.date.DateTime IHash (-hash [this] (tc/to-long this)))

(defn- day-of-week [date]
  (mod (+ 6 (.getDay date)) 7))

(def week-of-year (comp #(js/parseInt %) (js/d3.time.format "%W")))

(defn month-path [cell-size t0]
  (let [t1 (js/Date. (.getFullYear t0) (inc (.getMonth t0)) 0)
        d0 (day-of-week t0)
        d1 (day-of-week t1)
        w0 (week-of-year t0)
        w1 (week-of-year t1)]
    (apply format "M%d,%d H%dV%d H%dV%d H%dV%d H%dZ"
           (map (partial * cell-size)
                [(inc w0) d0
                 w0 7
                 w1 (inc d1)
                 (inc w1) 0
                 (inc w0)]))))

(defn calendar
"Render SVG calendar for multiple years."
  [data-per-date {:keys [width height margin cell-size color-class callback extent highlight-day tooltip-formatter]
                  :or {width 550
                       height 70
                       cell-size 9
                       color-class "RdYlGn"
                       tooltip-formatter (fn [date value] (str (human-date date) ": " value))
                       margin {:top 10 :right 10 :bottom 10 :left 10}}}] (when (not-empty data-per-date) (aset js/window "dpd" data-per-date))
  (when (not-empty data-per-date)
    (let [minmax (or extent (apply (juxt min max) (vals data-per-date)))
          [start-year end-year] (map #(.getFullYear %) (apply (juxt min max) (keys data-per-date)))
          color-scale (.. (js/d3.scale.quantize) (domain  (clj->js minmax)) (range (js/d3.range 8)))
          color-scale (fn [d] (or (color-scale d) 0)) ;; if extent is [0,0], color-scale always returns undefined
          ]
      [:div.calendar
       (for [year (range start-year (inc end-year))
             :let [width (max width (* cell-size 12 5))
                   height (max height (* cell-size 7))
                   tr (format "translate(%f,%f)"
                              (+ (:left margin)
                                 (/ (- width (* 53 cell-size)) 2))
                              (+ (:top margin)
                                 (/ (- height (* cell-size 7)) 2)))]]
         ^{:key year}
         [:svg {:width (+ width (:right margin) (:left margin))
                :height (+ height (:top margin) (:bottom margin))
                :class color-class
                                        ; use only one click listener instead of one per daily rect
                :on-click (when callback
                            #(let [target (.-target %)]
                               (when (and (= "rect" (.-nodeName target))
                                          (.hasAttribute target "data-date"))
                                 (callback (parse-human-date (.getAttribute target "data-date"))))))}
          [:g {:transform tr}
           [:g.label 
            [:text {:transform (format "translate(-6,%f)rotate(-90)" (* 3.5 cell-size))
                    :text-anchor "middle"} year]]
           [:g.days {:style {:opacity (when highlight-day 0.5)}}               
            (for [day ($ js/d3 time.day.utc.range (js/Date. year 0 1) (js/Date. (inc year) 0 1))
                  :let [day-str (human-date (tc/to-date-time day))
                        d (or (get data-per-date day)
                              (get data-per-date (tc/to-date-time day)))]]
              ^{:key day-str}
              [:rect.day {:width cell-size
                          :height cell-size
                          :x (* cell-size (week-of-year day))
                          :y (* cell-size (day-of-week day))
                          :data-date day-str
                          :class (str/join " " [(when (and callback d) "clickable")
                                                (when d (format "day q%d-9" (color-scale d)))])
                          :style {:fill (when (not d) "none")}}
               [:title (if d (tooltip-formatter (tc/to-date-time day) d) day-str)]])]
           [:g.months
            (for [date ($ js/d3 time.months (js/Date. year 0 1) (js/Date. (inc year) 0 1))
                  :let [month (.getMonth date)]]
              ^{:key (str month)}
              [:path.month {:d (month-path cell-size date)
                            :style {:stroke-opacity (when highlight-day (if (and (= year (.getFullYear highlight-day))
                                                                                 (= month (.getMonth highlight-day)))
                                                                          1.0
                                                                          0.2))}}])]
           (when (and highlight-day (= year (.getFullYear highlight-day)))
             (let [day-str (human-date highlight-day)
                   d (get data-per-date highlight-day)]
               [:g.highlight
                [:rect.day {:width cell-size
                            :height cell-size
                            :x (* cell-size (week-of-year highlight-day))
                            :y (* cell-size (day-of-week highlight-day))
                            :data-date day-str
                            :class (when d (format "day q%d-9" (color-scale d)))
                            :style {:fill (when (not d) "none")
                                    :stroke-width 3
                                    :stroke "black"}}
                 [:title (if d (tooltip-formatter (tc/to-date-time highlight-day) d) day-str)]]]))]])])))


(defn date-range-calendar [start-date end-date opts]
  (let [callback (fn [date-str]
                   (println date-str))
        start-date (tc/to-date-time start-date)
        end-date (tc/to-date-time end-date)
        days (tp/periodic-seq start-date end-date (t/days 1))
        data (reduce (fn [m day] (assoc m (tc/to-date day) 1)) {} days)]
    [calendar data (assoc opts :callback callback
                          :extent [0 1])]))
