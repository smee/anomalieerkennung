(ns gt1-ui.chart
  (:require [gt1-ui.remote :as remote]
            cljsjs.dygraph
            [cljs-time.coerce :as tc :refer [from-long to-date-time]]
            [cljs-time.core :as time :refer [date-time]]
            [cljs-time.format :as tf]
            [clojure.set :as set]
            [goog.dom :as dom]
            [goog.events :as ev]
            goog.style
            [gt1-ui.table :as table]
            [gt1-ui.defaults :refer [status-codes]]
            [gt1-ui.util :refer [completely-within-mode]]
            [gt1-ui.pages.common :refer [date-time->str]]
            [reagent.core :as reagent]
            [reagent.format :refer [format]]
            reagent-forms.datepicker
            [reagent.interop :refer-macros [$ $!]]
            [reagent.session :as session]
            [clojure.string :as str]))

(def chart-colors ["#e41a1c" "#377eb8" "#4daf4a" "#984ea3" "#ff7f00" "#999900" "#a65628" "#f781bf" "#999999"])
(def default-scale {:min 0 :max 1 :extent 1})

(defn datemap->date-time [{:keys [year month day hour minute second millis] :or {minute 0 hour 0 second 0 millis 0}}]
  (date-time year month day hour minute second millis))


(defn- highlights-draw-fn [highlights]
  (fn [canvas area g]
    (let [alpha ($ canvas -globalAlpha)]
      ($! canvas -globalAlpha 0.3)
      (doseq [{:keys [label start end color]} highlights
              :let [[x1] ($ g toDomCoords start 0)
                    [x2] ($ g toDomCoords end 0)
                    span (max 5 (- x2 x1))
                    top ($ area -y)
                    height ($ area -h)]]
        ($! canvas -fillStyle color)
        ($ canvas fillRect x1 top span height))
      ($! canvas -globalAlpha alpha))))

(defn- scaled? [scales]
  (not (every? #(= % default-scale) (vals scales))))

(defn line-chart
""
  [data {:keys [labels zoom-callback units highlights themes scales]
         :or {units {}
              zoom-callback (fn [_ _ _ _])
              scales {}}}]
  (let [watcher (reagent/atom nil)
        resize-fn (fn [this] ;; watch the width of our parent DOM element. Set our own width accordingly, so Dygraph will resize
                    (let [node (reagent/dom-node this)
                          dim (goog.style/getSize (dom/getParentElement node))
                          chart (:chart (reagent/state this))]
                      #_(goog.style/setHeight node ($ dim -height))
                      (goog.style/setWidth node (- ($ dim -width) 50))))
        dfmt (tf/formatter "dd.MM.yyyy HH:mm:ss")
        vf-gen (fn [units scales]
                 (fn [y opts series-name]
                   (if (= series-name "Zeit")
                     (tf/unparse dfmt (from-long y))
                     (let [{:keys [min extent]} (get scales series-name default-scale)]
                       (format "%.2f%s" (+ min (* y extent)) (units series-name ""))))))
        cb (fn [this] ;; create a callback that adds a `zoomed?` flag
             (fn [xmin xmax y-ranges]
               (zoom-callback xmin xmax y-ranges ($ (:chart (reagent/state this)) isZoomed "x"))))]
    (reagent/create-class
     {:component-will-update
      (fn [this [_ data {:keys [labels zoom units scales highlights series themes]}]]
        (let [chart (:chart (reagent/state this))
              labels (cons "Zeit" labels)
              opts (cond-> {:file (clj->js data)
                            :labels labels
                            :customBars (sequential? (second (first data)))
                            :dateWindow (when-let [{:keys [start-date end-date]} zoom]
                                          #js [(-> start-date datemap->date-time tc/to-long)
                                               (->   end-date datemap->date-time tc/to-long)])
                            :valueFormatter (vf-gen units scales)
                            :series themes
                            :underlayCallback (highlights-draw-fn highlights)
                            ;; drawAxis: false is not enough, canvas doesn't get cleared before drawing axis, ugly artifacts
                            :axes (when (scaled? scales) {:y {:axisLabelFormatter (constantly "")}})}
                     (not= (vec labels) (vec ($ chart getLabels))) (assoc :valueRange nil)
                     series (assoc :series series))]
          ($ chart updateOptions 
             (clj->js opts))))
      :component-did-mount
      (fn [this]
        (reset! watcher (ev/listen js/window "resize" #(resize-fn this)))
        (resize-fn this)
        (let [dom (reagent/dom-node this)
              element (aget (.getElementsByClassName dom "lineChart") 0)
              legend (aget (.getElementsByClassName dom "dygraph-legend") 0)
              chart (js/Dygraph. element
                                 (clj->js data)
                                 (clj->js {:labels (cons "Zeit" labels)
                                           :customBars (sequential? (second (first data)))
                                           :stepPlot true ; steps more representative of the nature of our 10min data
                                           :avoidMinZero true ; not necessarily include 0 in the y axis
                                           :connectSeparatedPoints true
                                           :axisLabelWidth 100
                                           :valueFormatter (vf-gen units scales)
                                           :zoomCallback (cb this)
                                           :labelsDivWidth 350
                                           :labelsSeparateLines true
                                           :labelsUTC true
                                           :legend "always"
                                           :labelsDiv legend
                                           :height 600
                                           :highlightSeriesOpts {:strokeWidth 2
                                                                 :strokeBorderWidth 1}
                                           :colors chart-colors
                                           :series themes
                                           :underlayCallback (highlights-draw-fn highlights)
                                           :axes (when (scaled? scales) {:y {:axisLabelFormatter (constantly "")}})}))]
          ($! chart -panCallback (cb this))
          (reagent/replace-state this {:chart chart})))
      :reagent-render (fn []
                        [:div
                         [:div.lineChart "Chart goes here"]
                         [:div.dygraph-legend]])})))

(defonce pan-handling
  ;"Replace builtin pan handling of Dygraphs, calls a zoom callback if present."
  (let [orig-handler js/Dygraph.Interaction.endPan]
    ($! js/Dygraph -Interaction.endPan
          (fn [event g context]
            (orig-handler event g context)
            ($! context -isPanning false)
            (when ($ g -panCallback)
              (let [x-range($ g xAxisRange)]
                ($ g panCallback (aget x-range 0) (aget x-range 1) ($ g yAxisRanges))))))
    ($! js/Dygraph -endPan ($ js/Dygraph -Interaction.endPan))))




(defn millis->map [millis]
  (clojure.set/rename-keys (tf/instant->map millis)
                           {:years :year
                            :months :month
                            :days :day
                            :minutes :minute
                            :hours :hour
                            :seconds :second
                            :millis :milli}))

(defn merge-chart-data [zoomed-dates old-data new-data]
  (if (or (not zoomed-dates)
          (not= (count (first new-data)) (count (first old-data))))
    new-data
    (let [oldest-detail (ffirst new-data)
          newest-detail (first (last new-data))
          before (take-while #(time/before? (first %) oldest-detail) old-data)
          after (drop-while #(time/before? (first %) newest-detail) old-data)]
      (reduce into [] [before new-data after]))))

(defn rescale
"Rescale each data point via `(/ (- value minimum) (- maximum minimum))`"
  [data labels rescale? avg-only?]
  (if (not rescale?)
    {:data data
     :scales (into {} (map vector labels (repeat default-scale)))}
    (let [label-by-index (into {} (map-indexed vector labels))
          extents (reduce (fn [stats [_ & vs]]
                            (mapv (fn [[mi ma] v]
                                    (if (not (vector? v))
                                      [mi ma]
                                      (let [[mi' avg ma'] v
                                            mi' (if avg-only? avg mi')
                                            ma' (if avg-only? avg ma')]
                                        [(if (js/Number.isFinite mi') (min mi mi') mi)
                                         (if (js/Number.isFinite ma') (max ma ma') ma)]))) stats vs))
                          (vec (repeat (count labels) [js/Number.POSITIVE_INFINITY js/Number.NEGATIVE_INFINITY])) data)
          scales (mapv (fn [[mi ma]] {:min mi :max ma :extent (if (= mi ma) 1.0 (- ma mi))}) extents)
          data (for [[ts & vs] data] (cons ts (map (fn [v sc]
                                                     (if (vector? v)
                                                       (let [[mi avg ma] v
                                                             f #(/ (- % (:min sc)) (:extent sc))] [(f mi) (f avg) (f ma)]) v)) vs scales)))
          scales (into {} (map vector labels scales))]      
      {:data data
       :scales scales})))

(defn generate-series-colors [plants selected-nodes] ;; TODO sticky colors, cache them so we don't see changing colors if series get deselected
  (let [colors (into {} (map vector (map :series selected-nodes) (cycle chart-colors)))
        ;; see http://blog.dygraphs.com/2012/02/custom-lines-new-feature.html
        strokes (into {} (map vector plants (cycle [[nil] [7 2 2 2] [2 2] [2 2 4 2 6 2]])))]
    (clj->js (into {} (for [p plants, {:keys [label series]} selected-nodes]
                        [(str p "/" label){:color (colors series)
                                           :strokePattern (strokes p)}])))))

(defn extract-averages [data]
  (mapv (fn [[ts & vs]] (into [ts] (map #(if (vector? %) (second %) %) vs))) data))

(defn remote-line-chart [plants selected-nodes state]
  (let [internal (reagent/atom {:xhr nil
                                :last-params {}
                                :values []
                                :labels []
                                :units {}
                                :last-args {}})
        zoom-callback (fn [min-date max-date y-ranges zoomed?]
                        (swap! state assoc :zoom (when zoomed?
                                                   {:start-date (millis->map (from-long min-date))
                                                    :end-date (millis->map (from-long max-date))})))
        request-params (fn [plants selected-nodes {z :zoom o :overall e :errors? m :modes} initial-request?]
                         (let [start (datemap->date-time (or (:start-date z) (:start-date o)))
                               end (datemap->date-time (or (:end-date z) (:end-date o)))]
                           (if (and (not-empty (:values @internal))
                                    (<= (time/in-seconds (time/interval start end))
                                       gt1-ui.defaults/minimum-time-interval))
                             (:last-params @internal)
                             {:series (mapv :series selected-nodes)
                              :plants (vec plants)
                              :values? true
                              :errors? initial-request?
                              :modes? initial-request?
                              :dates [(date-time->str start)
                                      (date-time->str end)]})))
        sync-data (fn [this [_ plants selected-nodes state]]
                    (let [lp (:last-params @internal)]
                      (if (or (empty? selected-nodes)
                              (empty? plants))
                        (do
                          (swap! state assoc :zoom nil)
                          (reset! internal {}))
                        ;; FIXME really need old/new state to decide this!
                        (let [args {:plants plants :selected-nodes (mapv :series selected-nodes) :overall (:overall @state)}
                              initial? (or (empty? (:modes @internal)) (not= (:last-args @internal) args))
                              params (request-params plants selected-nodes @state initial?)]
                          (when (not= params lp) ;; TODO if we are just missing things (less plants, series, modes/errors off) we can change our data locally without querying the backend 
                            (when (:xhr @internal)
                              (ajax.core/abort (:xhr @internal)))
                            (swap! internal assoc
                                   :last-params params
                                   :last-args args
                                   :xhr (remote/GET (str "/db/data")
                                            {:params params                                           
                                             :handler (fn [response]
                                                        (let [new-data (:values response)
                                                              old-data (:values @internal)
                                                              data (merge-chart-data (:zoom @state) old-data new-data)
                                                              labels (for [plant plants, label (map :label selected-nodes)] (str plant "/" label))
                                                              themes (generate-series-colors plants selected-nodes)
                                                              units (mapv :unit selected-nodes)
                                                              errors (if initial? (:errors response) (:errors @internal))
                                                              modes (if initial? (:modes response) (:modes @internal))]
                                                          (swap! internal assoc
                                                                 :xhr nil
                                                                 :values data
                                                                 :errors errors
                                                                 :modes modes
                                                                 :labels labels
                                                                 :themes themes
                                                                 :units (zipmap labels (cycle (map :unit selected-nodes))))))})))))))]
    
    (reagent/create-class
     {:component-did-mount (fn [this] (sync-data this [nil plants selected-nodes state])) ;; fetch initial data (:component-will-update only gets called on changes to the initial data!))
      :component-will-update sync-data
      :reagent-render 
      (fn [plants selected-nodes state]
        (let [{:keys [errors? modes? only-production? rescale? line-only? zoom overall]} @state
              labels (:labels @internal)
              units (:units @internal)
              themes (:themes @internal)
              {sz :start-date ez :end-date} zoom
              {so :start-date eo :end-date} overall
              start-time (datemap->date-time (or sz so))
              end-time  (datemap->date-time (or ez eo))
              in-range? #(time/overlaps? start-time end-time (:timestamp %) (:end %)) 
              errors (into {} (for [[k vs] (:errors @internal)]
                                [k (filter in-range? vs)]))
              modes (filter in-range? (:modes @internal))           
              visible-interval (time/interval start-time end-time)
              data (:values @internal [])
              nan js/Number.NaN
              data (if (and (= 1 (count plants)) only-production?)
                     (let [intervals (completely-within-mode 7 modes)
                           f (fn [[date]] (some (fn [[from to]] (time/within? from to date)) intervals ))]
                       (->> data
                            (partition-by f)
                            (mapcat (fn [[d :as vs]] (if (f d) vs [(cons (first d) (repeat (dec (count d)) [nan nan nan]))])))
                            vec))                     
                     data)
              {:keys [data scales]} (rescale data labels rescale? line-only?)
              data (if line-only? (extract-averages data) data)
              highlights (cond-> []
                           errors? (into (for [[plant vs] errors,
                                               {s :timestamp, e :end c :component id :error-id} vs]
                                           {:label (str plant ": " c " " id)
                                            :start s
                                            :end e
                                            :color "orange"}))
                           modes? (into (for [{:keys [timestamp end mode]} modes]
                                          {:label (get-in status-codes [mode :name])
                                           :color (get-in status-codes [mode :color] "white")
                                           :start timestamp
                                           :end end})))]
          [:div.col-md-12
           [:div.row 
            [:div.col-md-12
             (when (and (not-empty plants)
                        (not-empty labels))
               [line-chart data
                           {:labels labels
                            :scales scales
                            :highlights highlights
                            :units units
                            :zoom (:zoom @state)
                            :themes themes
                            :zoom-callback zoom-callback}])
             (when (and errors? (not-empty errors))
               [table/error-table errors])
             (when (and modes? (not-empty modes))
               [table/modes-table (first plants) modes])]]]))})))
 
