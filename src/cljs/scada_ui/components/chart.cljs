(ns scada-ui.components.chart
  (:require cljsjs.dygraph
            [cljs-time.coerce :as tc :refer [from-long]]
            [cljs-time.core :as time :refer [date-time]]
            [cljs-time.format :as tf]
            [clojure.set :as set]
            [clojure.data :refer [diff]]
            [goog.dom :as dom]
            [goog.events :as ev]
            [goog.object :as obj]
            [goog.Timer :as timer]
            goog.style
            [scada-ui.components.table :as table]
            [scada-ui.components.heatmap :as hm]
            [scada-ui.remote :as remote]
            [scada-ui.format :as f]
            [scada-ui.state :as state]
            [scada-ui.pages.common :as pc]
            [scada-ui.util :as util]
            [scada-ui.i18n :refer [i18n]]
            [reagent.core :as reagent]
            [reagent.format :refer [format]]
            [reagent.interop :refer [$ $!]]
            [reagent.ratom :refer [run!]]
            [re-frame.core :refer [reg-event-db path reg-sub dispatch subscribe]]
            [clojure.string :as str]
            [scada-ui.pages.common :as pc]
            [scada-ui.components.bootstrap :as bs]))

(def chart-colors ["#e41a1c" "#377eb8" "#4daf4a" "#984ea3" "#ff7f00" "#999900" "#a65628" "#f781bf" "#999999"])
(def default-scale {:min 0 :max 1 :extent 1})
(def middleware [#_re-frame.core/debug])

;;;;;;;;;;;;;;;;; chart data manipulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-last-interval [data]
  (let [[[ts1] [ts2 & vs2]] (take-last 2 data)] 
    (if ts2
      (conj (vec data) (cons (time/plus ts2 (time/seconds (time/in-seconds (time/interval ts1 ts2)))) vs2))
      data)))

(defn to-dygraphs
"{{:keys [name series]} [[timestamp & [min, avg,max]]} to [[timestamp & [min, avg,max] or NaN per series]]"
  [order m]
  (let [gap [js/Number.NaN js/Number.NaN js/Number.NaN]]
    ;; TODO think about: we need NaN to signify a gap in the data. For trace data we have sensors without traces, where we want to show the current time intervals value (need null for trace data timestamps)
    (loop [m m, res []]
      (if (every? empty? (vals m))
        (add-last-interval res)
        (let [entries (map #(first (get m %)) order)
              timestamps (map first entries)
              date (time/earliest (keep identity timestamps))
              row (into [date] (for [[ts vs] entries]
                                 (cond
                                   (nil? ts) gap
                                   (time/equal? date ts) vs
                                   :else gap)))]
          (recur (reduce-kv (fn [res k [[ts] :as rows]]
                              (assoc res k (if (or (nil? ts) (not (time/equal? date ts))) rows (next rows))))
                            {} m)
                 (conj res row)))))))

(defn merge-chart-data [old-data new-data]
  (reduce-kv (fn [res k old-data]
               (if-let [new-data (get new-data k)]
                 (let [oldest-detail (ffirst new-data)
                       newest-detail (first (last new-data))
                       before (take-while #(time/before? (first %) oldest-detail) old-data)
                       after (drop-while #(time/before? (first %) newest-detail) old-data)]
                   (assoc res k (reduce into [] [before new-data after])))
                 (assoc res k old-data)))
             {} old-data))

(defn rescale
"Rescale each values point via `(/ (- value minimum) (- maximum minimum))`"
  [values labels focus-interval average-only?]
  (let [label-by-index (into {} (map-indexed vector labels))
        extents (reduce (fn [stats [time & vs]]
                          (mapv (fn [[mi ma] v]
                                  (if (or (not (time/within? focus-interval time)) (not (vector? v)))
                                    [mi ma]
                                    (let [[mi' avg ma'] v
                                          mi' (if average-only? avg mi')
                                          ma' (if average-only? avg ma')]
                                      [(if (js/isFinite mi') (min mi mi') mi)
                                       (if (js/isFinite ma') (max ma ma') ma)]))) stats vs))
                        (vec (repeat (count labels) [js/Number.POSITIVE_INFINITY js/Number.NEGATIVE_INFINITY])) values)
        scales (mapv (fn [[mi ma]] {:min mi :max ma :extent (if (= mi ma) 1.0 (- ma mi))}) extents)
        values (vec
                (for [[ts & vs] values]
                  (into [ts] (map (fn [v sc]
                                    (if (vector? v)
                                      (let [[mi avg ma] v
                                            f #(/ (- % (:min sc)) (:extent sc))] [(f mi) (f avg) (f ma)]) v)) vs scales))))
        scales (into {} (map vector labels scales))]      
    {:values values
     :scales scales}))

(defn averages-only [data]
  (mapv (fn [[ts & vs]] (into [ts] (map (fn [[min avg max]] [avg avg avg]) vs))) data))

;;;;;;;;;;;;;;;;;;;;;; subscriptions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(state/reg-sub :charts/show-pls? [:chart :visibility :show-pls?])
(reg-event-db :charts/toggle-show-pls? [(path [:chart :visibility :show-pls?]) middleware]
              (fn [show? [_]]
                (not show?)))

(reg-event-db :charts/toggle-series-visibility [(path [:chart]) middleware]
              (fn [db [_ label]]
                (update-in db [:hidden label] not)))

(state/reg-sub :charts/series-hidden? [:chart :hidden])

;; need to duplicate subscriptions because charts-page referes to this namespace, so we would use
;; subscriptions before they were defined
(state/reg-sub :charts/state [:chart])
(state/reg-sub :charts/flags [:chart :flags])

(reg-sub :charts/time-interval
              (fn [db [_ key]]
                (if (#{:overall :zoom} key)
                  (select-keys (get-in db [:chart key]) [:start :end])
                  (throw (ex-info "there is no timeinterval with this key" {:key key})))))

(reg-sub :charts/zoom-interval
         :<- [:charts/time-interval :overall]
         :<- [:charts/time-interval :zoom]
         (fn [[{so :start eo :end} {sz :start ez :end}]] 
           (if (and sz ez)
             {:start sz :end ez}
             {:start so :end eo})))

;; what is the currently visible time range? create a function that can filter all kinds of event lists
(reg-sub :charts/in-range?
         :<- [:charts/zoom-interval]
         (fn [{:keys [start end]}]
           (fn [{from :timestamp to :end}]
             ;; for instantaneous parameter changes there is no time interval, only a time instant
             (time/overlaps? start end from (or to from)))))

(state/reg-sub :charts/raw-errors  [:chart :errors])
(state/reg-sub :charts/raw-changes [:chart :changes])
(state/reg-sub :charts/raw-modes   [:chart :modes])

(reg-sub :charts/filtered-modes
         :<- [:charts/in-range?]
         :<- [:charts/raw-modes]
         (fn [[in-range? modes]] (vec (filter in-range? modes))))

(reg-sub :charts/modes
         :<- [:operation-modes]
         :<- [:charts/filtered-modes]
         (fn [[op-modes rows]]
           (mapv #(assoc (merge % (op-modes (:id %))) :duration (time/interval (:timestamp %) (:end %))) rows)))

(reg-sub :charts/filtered-changes
         :<- [:charts/in-range?]
         :<- [:charts/raw-changes]
         :<- [:charts/show-pls?]
         (fn [[in-range? changes show-pls?]]
           (vec (filter #(and (in-range? %)
                              (or show-pls? (not= "PLSMB" (:user %))))
                        changes))))

(reg-sub :charts/changes
         :<- [:change-details]
         :<- [:charts/filtered-changes]
         (fn [[details-fn changes]]
           (mapv (fn [c] (merge c (details-fn (:id c)))) changes)))

(reg-sub :charts/filtered-errors
         :<- [:charts/in-range?]
         :<- [:charts/raw-errors]
         (fn [[in-range? errors]]
           (vec (filter in-range? errors))))


(reg-sub :charts/highlights
         :<- [:charts/flags]
         :<- [:charts/filtered-changes]
         :<- [:charts/filtered-modes]
         :<- [:charts/filtered-errors]
         :<- [:operation-modes]
         (fn [[{:keys [changes? errors? modes?]} changes modes errors operation-modes]]
           (cond-> []
             changes? (into (for [{:keys [timestamp end]} changes]  {:start timestamp :end end :color "green"}))
             errors?  (into (for [{:keys [timestamp end]} errors]   {:start timestamp :end end :color "orange"}))
             modes?   (into (for [{:keys [timestamp end id]} modes] {:start timestamp :end end :color (get-in operation-modes [id :color] "white")}))
             true (->> (sort-by :start) vec))))

(reg-sub :charts/zoomed?
         :<- [:charts/state]
         (fn [state] (-> state :zoom :start boolean)))

(defn series-label [name->label plant sensor]
  (str plant "/" (name->label sensor)))

(reg-sub :charts/heatmap
         :<- [:name->label]
         :<- [:charts/state]
         :<- [:charts/zoomed?]
         (fn [[name->label state zoomed?]]
           (let [{:keys [x-series y-series] :as data} (get-in state [(if zoomed? :zoom :overall) :heatmap])]
             (assoc data :x-label (series-label name->label (:plant x-series) (:series x-series))
                    :y-label (series-label name->label (:plant y-series) (:series y-series))))))

(reg-sub :charts/overall-values (fn [db] (get-in db [:chart :overall :values])))

(reg-sub :charts/zoom-values (fn [db] (get-in db [:chart :zoom :values])))

(state/reg-sub :charts/traces [:chart :zoom :traces])

(reg-sub :charts/series-order :<- [:charts/overall-values]
         (fn [m]
           (sort-by (juxt :plant :series) (keys m))))

(reg-sub :charts/labels
         :<- [:name->label]
         :<- [:charts/overall-values]
         :<- [:charts/series-order]
         (fn [[name->label values order]]
           (let [labels (map (fn [{:keys [plant series]}]
                               (series-label name->label plant series))
                             order)]
             ;; if multiple series have the same name, add numeric suffixes
             (first
              (reduce (fn [[res freqs] label]
                        (let [n (freqs label)]
                          (if (= 1 n)
                            [(conj res label) freqs]
                            [(conj res (str label " (" n ")")) (update freqs label dec)])))
                      [[] (frequencies labels)] labels)))))

(reg-sub :charts/units
         :<- [:charts/labels]
         :<- [:id->node]
         :<- [:charts/series-order]
         (fn [[labels s->s order]]
           (->> order
                (map :series)
                (map #(:unit (s->s %) ""))
                (zipmap labels))))

(reg-sub :charts/themes
         :<- [:charts/labels]
         :<- [:charts/series-order]
         :<- [:charts/series-hidden?]
         (fn [[labels order hidden?]]
           ;; TODO needs more work to keep color when adding/removing columns
           (let [series (map #(assoc %1 :label %2) order labels)
                 colors (zipmap (distinct (map :series series)) (cycle chart-colors))
                 ;; see http://blog.dygraphs.com/2012/02/custom-lines-new-feature.html
                 strokes (zipmap (distinct (map :plant series)) (cycle [[nil] [4 4] [3 3] [2 2]]))] 
             (clj->js (into {} (for [{p :plant series :series label :label} series]
                                 [label {:color (if (hidden? label) "#c0c0c0" (colors series))
                                         :strokePattern (strokes p)}]))))))
(reg-sub :charts/included-zoom-values
         :<- [:charts/zoomed?]
         :<- [:charts/overall-values]
         :<- [:charts/zoom-values]
         (fn [[zoomed? ov zv]]
           (if (and zoomed? (not-empty zv))
             (merge-chart-data ov zv)
             ov)))

(defn split-traces
  "If we have multiple close traces, we want to keep the coarse data in between!
To do that we split the data into individual segments (that is, no two data points are 
longer than `max-time-diff` seconds apart)."
  [traces max-time-diff]
  (reduce-kv
   (fn [res series vs]
     (->> vs
          (partition 2 1)
          (partition-by (fn [[[ts1] [ts2]]]
                          (>= (time/in-seconds (time/interval ts1 ts2)) max-time-diff)))
          (map (partial mapv second))
          (remove #(= 1 (count %)))
          (reduce #(conj %1 {series %2}) res)))
   [] traces))

;; do we have zoom data that reaches our maximum sampling rate, i.e.: is there no more zoom data to fetch?
(reg-sub :charts/high-resolution?
         :<- [:charts/time-interval :zoom]
         :<- [:sampling-rate]
         (fn [[{:keys [start end]} sampling-rate]]
           (and start end (<= (time/in-seconds (time/interval start end)) (* 50 sampling-rate)))))

(reg-sub :charts/included-trace-values
         :<- [:charts/included-zoom-values]
         :<- [:charts/traces]
         :<- [:sampling-rate]
         :<- [:charts/high-resolution?]
         (fn [[data traces sampling-rate high-res?]]
           (if (and (not-empty traces) high-res?)
             (let [individual-traces (split-traces traces sampling-rate)]
               (reduce merge-chart-data data individual-traces))
             data)))

(reg-sub :charts/dygraph-values
         :<- [:charts/flags]         
         :<- [:charts/series-order]
         :<- [:charts/included-trace-values]
         (fn [[{:keys [line-only?]} order data]]
           (let [values (to-dygraphs order data)]                 
             (if line-only?
               (averages-only values)
               values))))

(reg-sub :charts/scaled-dygraph-values
         :<- [:charts/dygraph-values]
         :<- [:charts/labels]
         :<- [:charts/zoom-interval]
         :<- [:charts/flags]
         (fn [[values labels {:keys [start end]} {:keys [line-only? rescale?]}]]
           (if rescale?
             (rescale values labels (time/interval start end) line-only?)
             {:values values
              :scales (into {} (map vector labels (repeat default-scale)))})))

(reg-sub :charts/heatmap-extents :<- [:charts/state] (fn [state] (get-in state [:overall :heatmap :extents])))

(reg-sub :charts/new-request
   :<- [:selected-series]
   :<- [:charts/high-resolution?]
   :<- [:charts/time-interval :overall]
   :<- [:charts/time-interval :zoom]
   :<- [:charts/flags]
   :<- [:charts/modes]   
   (fn [[series high-res? overall zoom flags modes]]
     {:series (set series)
      :overall overall
      :zoom zoom
      :only-production? (:only-production? flags)
      :max-resolution? high-res?
      :events {:start (:timestamp (first modes))
               :end (:end (last modes))}}))

(defn- fetch-data! [old-req {:as new-req :keys [max-resolution? only-production?]}]
  (let [interesting-keys [:series :overall :zoom :only-production?]
        [removed added] (diff (select-keys old-req interesting-keys) (select-keys new-req interesting-keys))        
        all-series (:series new-req)            
        any? (not-empty all-series)
        {zs :start ze :end} (:zoom new-req)
        {zs' :start ze' :end} (:zoom added)
        {os :start oe :end} (:overall new-req)
        {os' :start oe' :end} (:overall added)
        changed (first (keys added))
        zoomed? (and zs ze)
        traces? (and (not only-production?) max-resolution?)
        fetch (fn [args params]
                (let [params' (merge {:only-production? only-production?
                                      :traces? false
                                      :values? true} params)]
                  #_(println "fetching" params')
                  (remote/run-request {:url "/db/data"
                                       :method :post
                                       :success [:charts/received-values args]
                                       :params params'})))]
    (when (not (and (empty? added) (empty? removed)))
      #_(js/console.debug "called fetch-data!" {:any? any? :|added| (count added) :added added :removed removed :changed changed :zoomed? zoomed? :traces? traces?})
      (if (and any? (or (> (count added) 1) os' oe')) ;; multiple batched dispatches change in overall date range? just disregard current data, fetch all 
        (do ;; fetch all data for overall date range, remove zoom data
          (fetch #{:replace :overall} {:series all-series :start os :end oe :errors? true :modes? true :changes? true})
          (dispatch [:charts/remove-zoom-data]))
        ;; only a single element has changed, so let's find out what exactly and then do the smallest requests possible
        (do ;; to reduce server stress, only fetch what is needed in addition to currently present data        
          ;; series deselected?
          (when-let [del-series (:series removed)] (dispatch [:charts/remove-series del-series]))
          ;; newly selected series, add to already present ones
          (when-let [added-series (:series added)]
            (fetch #{:merge :overall} {:series added-series :start os :end oe :modes? true :errors? true :changes? true})
            (when zoomed? (fetch #{:merge :zoom} {:series added-series :start zs :end ze :traces? traces?})))
          ;; :only-production? has changed
          (when (= changed :only-production?)
            (fetch #{:merge :overall} {:series all-series :start os :end oe})
            (when zoomed? (fetch #{:merge :zoom} {:series all-series :start zs :end ze :traces? traces?})))
          ;; change in zoom date range? 
          (when (or zs' ze')
            (let [event-start (-> new-req :events :start)
                  event-end (-> new-req :events :end)]
              (if (not (and event-start event-end)) ;; no modes etc, just fetch everything
                (fetch #{:merge :zoom} {:series all-series :start zs :end ze :modes? true :changes? true :errors? true})
                (do ;; fetch modes, changes, errors if new date range exceed currently available data, but do not fetch values
                  (fetch #{:merge :zoom} {:series all-series :start zs :end ze :traces? traces?})
                  (doseq [interval (util/time-interval-differences (time/interval event-start event-end) (time/interval zs ze))]
                    (fetch #{:merge :zoom} {:series all-series :start (time/start interval) :end (time/end interval) :modes? true :changes? true :errors? true :values? false :traces? false})))))))))))

;;;;;;;;;;;;;;;;;;;;;; logic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; changeable by user: overall, zoom, selected nodes, selected plants, flags
;; needs backend interaction: all but flags
(reg-event-db :charts/zoom (path [:chart :zoom])
   (fn [db [_ start end zoomed?]]
     (assoc db :start (when zoomed? start) :end (when zoomed? end))))

(defn detect-gaps [values]
  (->> ;; insert NaN if there is a gap in the data of a series (i.e. time gap > 2*median time gap)
   (for [[k rows] values
         :let [diffs (->> rows
                          (map first)
                          (partition 2 1)
                          (map (fn [[a b]] (time/in-seconds (time/interval a b))))
                          sort)
               |diffs| (count diffs)]
         :when (not (zero? |diffs|))
         :let [median-idx (int (/ |diffs| 2))
               median (nth diffs median-idx)]]
     [k (:res
         (reduce (fn [{:keys [res previous]} [ts :as row]]
                   {:previous ts
                    :res (if (and previous (> (time/in-seconds (time/interval previous ts)) (* 2 median)))
                           (conj res [(time/plus previous (time/millis 1)) [js/Number.NaN js/Number.NaN js/Number.NaN]] row)
                           (conj res row))}) {:res [] :previous nil} rows))])
   (into {})))

(defn- merge-lists
"Two ordered lists of items with keys `:timestamp`, merges them so the result
contains both lists without duplicates."
  [l1 l2]
  (cond
    (empty? l1) l2
    (empty? l2) l1
    :else (vec (util/merge-events (sort-by :timestamp (distinct (concat l1 l2)))))))

;; remove deselected series from app-db, no need to rerun queries
(reg-event-db :charts/remove-series [(path [:chart]) middleware]
   (fn [db [_ series-keys]]
     (let [del #(apply dissoc % series-keys)]
       (-> db
           (update-in [:overall :values] del)
           (update-in [:overall :traces] del)
           (update-in [:zoom :values] del)
           (update-in [:zoom :traces] del)))))

;; delete zoom-data
(reg-event-db :charts/remove-zoom-data [(path [:chart :zoom]) middleware]
   (fn [db]
     (dissoc db :values :traces)))

;; put new chart data from backend into our application state
(reg-event-db :charts/received-values [(path [:chart]) middleware]
   (fn [db [_ change {:keys [modes errors values changes traces]}]]
     (let [values (detect-gaps values)
           zoom? (change :zoom)
           replace? (change :replace)
           merge? (change :merge)] ;; TODO remove trace data out of zoom range
       (cond-> db
         merge? (-> (update-in [:errors] merge-lists errors)
                    (update-in [:modes] merge-lists modes)
                    (update-in [:changes] merge-lists changes)
                    (update-in [(if zoom? :zoom :overall) :values] merge values)
                    (update-in [:zoom :traces] merge traces))
         replace? (-> (assoc-in [:zoom :values] {})
                      (assoc-in [:zoom :traces] {})
                      (assoc-in [:overall :values] values)
                      (assoc-in [:overall :traces] traces)
                      (assoc :errors errors :modes modes :changes changes))))))

(reg-event-db :charts/received-heatmap [(path [:chart]) middleware]
   (fn [db [_ key data]]
     (assoc-in db [key :heatmap] data)))


;;;;;;;;;;;;;;;;;; rendering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- highlights-draw-fn [highlights]
  (fn [canvas area g]
    (let [alpha ($ canvas -globalAlpha)]
      ($! canvas -globalAlpha 0.3)
      (doseq [{:keys [start end color]} highlights
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

(def google-maps-interaction
  (let [focus-canvas (fn [canvas] ;; browsers automatically scroll to get focussed elements into the viewport. We don't want that to happen for our canvas focus trick. Instead, remember old scroll position, restore it after enabling focus
                       (let [x (.-scrollX js/window)
                             y (.-scrollY js/window)]
                         (.focus canvas)
                         (.scrollTo js/window x y)))
        zoom-cb (pc/debounce-factory)
        scroll-fn (fn [event g context]
                    (.preventDefault event)
                    (let [detail ($ event -detail)
                          wd ($ event -wheelDelta)
                          direction (if (or (< detail 0) (> wd 0)) 1 -1)
                          offset-x (or (.-offsetX event) (- (.-layerX event) (.-offsetLeft (.-target event))))
                          [left right] ($ g xAxisRange)
                          x-left (aget ($ g toDomCoords left nil) 0)
                          x-right (aget ($ g toDomCoords right nil) 0)
                          x (- offset-x x-left)
                          w (- x-right x-left)
                          percentage (if (zero? w) 0 (double (/ x w)))
                          time-delta (- right left)
                          zoom-factor 0.25
                          incr (* time-delta (* direction zoom-factor))
                          new-left (+ left (* incr percentage))
                          new-right (- right (* incr (- 1 percentage)))]
                      (zoom-cb #(dispatch [:charts/zoom (tc/to-date-time new-left) (tc/to-date-time new-right) true]) 300)
                      ($ g updateOptions (clj->js {:dateWindow [new-left new-right]}))))]
    {:mousedown (fn [event g context]
                  ($ context initializeMouseDown event g context)
                  (if (or (.-altKey event) (.-shiftKey event))
                    ($ js/Dygraph startZoom event g context)
                    ($ js/Dygraph startPan event g context)))
     :keydown (fn [event g context]
                (let [key-code (.-keyCode event)
                      up? (= key-code 38)
                      down? (= key-code 40)
                      left? (= key-code 37)
                      right? (= key-code 39)                      
                      [left right] ($ g xAxisRange)
                      to-shift (* 0.33 (- right left))
                      left' (+ left (* (cond (or left? down?) -1 (or right? up?) 1) to-shift))
                      right' (+ right (* (cond (or left? up?) -1 (or right? down?) 1) to-shift))
                      zoom? (and (not (.-altKey event)) (or left? right? up? down?))]
                  (when zoom?
                    (.preventDefault event)
                    (zoom-cb #(dispatch [:charts/zoom (tc/to-date-time left') (tc/to-date-time right') true]) 300))))     
     ;; canvas can't have focus by default, enforce it so we can listen to keyboard events
     :mouseover (fn [event]
                  (focus-canvas (.-target event)))
     :mouseout (fn [event]
                 (.blur (.-target event)))
     :mouseup (fn [event g context]
                (cond
                  ($ context :isPanning) ($ js/Dygraph endPan event g context)
                  ($ context :isZooming) ($ js/Dygraph endZoom event g context)))
     :mousemove (fn [event g context]
                  (focus-canvas (.-target event))
                  (cond
                    ($ context :isPanning) ($ js/Dygraph movePan event g context)
                    ($ context :isZooming) ($ js/Dygraph moveZoom event g context)))
     :dblclick (fn [event g context]
                 (dispatch [:charts/zoom nil nil false])
                 ($ g updateOptions (clj->js {:valueRange nil})))
     :mousewheel scroll-fn
     :DOMMouseScroll scroll-fn}))

(defn- point-clicked [event point]
  (js/console.log "point" point))

(defn points-clicked [event x points]
  (js/console.log "points" x points))


(defonce legend-formatter-fn  
  ;; in v1.1.1 there is no explicit way of configuring legend items
  ;; so we need to hack it in...
  ;; FIXME needs to be changed if dygraphs gets updated!
  (let [fn-name "generateLegendHTML"
        legend-plugin (first (filter #(.hasOwnProperty % fn-name) (seq (obj/get js/Dygraph "PLUGINS"))))
        orig-fn (obj/get legend-plugin fn-name)
        hidden? (subscribe [:charts/series-hidden?])]
    (obj/set js/window "__legendItemToggle"
             (fn [label] (dispatch [:charts/toggle-series-visibility label])))
    (obj/set legend-plugin fn-name
             (fn [g x sel_points oneEmWidth row]
               (let [html (orig-fn g x sel_points oneEmWidth row)
                     labels (-> g ($ getLabels) seq next)]
                 (if sel_points
                   html
                   (reduce (fn [html label]
                             (str/replace html (str label "</span>") (f/format "<input type=\"checkbox\" onChange=\"__legendItemToggle('%s')\" %s>%s</span>" label (if (@hidden? label) "" "checked") label)))
                           html labels)))))
    ::initialized))

(defn line-chart
  "Line chart component, uses Dygraphs."
  [data {:keys [labels zoom-callback zoom units highlights themes scales interaction]
         :or {units {}
              zoom-callback (fn [_ _ _ _])
              scales {}}}]
  (let [watcher (reagent/atom nil)
        resize-fn (fn [this] ;; watch the width of our parent DOM element. Set our own width accordingly, so Dygraph will resize correctly
                    (let [node (reagent/dom-node this)
                          dim (goog.style/getSize (dom/getParentElement node))
                          chart (:chart (reagent/state this))]
                      #_(goog.style/setHeight node ($ dim -height))
                      (goog.style/setWidth node (- ($ dim -width) 50))))
        dfmt (tf/formatter "dd.MM.yyyy HH:mm:ss")
        dfmt-ms (tf/formatter "dd.MM.yyyy HH:mm:ss.SSS")
        vf-gen (fn [units scales]
                 (fn [y opts series-name]
                   (if (= series-name (i18n ["Zeit"]))
                     (let [l (from-long y)
                           ;; show milliseconds if there are non-zero digits
                           fmt (if (zero? (mod l 1000)) dfmt dfmt-ms)]
                       (tf/unparse fmt l))
                     (let [{:keys [min extent]} (get scales series-name default-scale)]
                       (format "%s%s" (f/format-number (+ min (* y extent))) (units series-name ""))))))
        cb (fn [this] ;; create a callback that adds a `zoomed?` flag
             (fn [xmin xmax y-ranges]
               (zoom-callback xmin xmax y-ranges true)))
        date-window (fn [{:keys [start end]}]
                      (when (and start end)
                        #js [(-> start tc/to-long)
                             (-> end tc/to-long)]))]
    (reagent/create-class
     {:component-will-update
      (fn [this [_ data {:keys [labels zoom units scales highlights themes]}]]
        (let [chart (:chart (reagent/state this))
              labels (cons (i18n ["Zeit"]) labels)
              opts (cond-> {:file (clj->js data)
                            :labels labels
                            :customBars (sequential? (second (first data)))                            
                            :dateWindow (date-window zoom)
                            :valueFormatter (vf-gen units scales)
                            :series themes
                            :highlightSeriesBackgroundAlpha (if (>= (count labels) 4) 0.3 0.5)
                            :underlayCallback (highlights-draw-fn highlights)
                            ;; drawAxis: false is not enough, canvas doesn't get cleared before drawing axis, ugly artifacts
                            :axes (if (scaled? scales)
                                    {:y {:axisLabelFormatter (constantly "")}}
                                    {:y {:axisLabelFormatter #(f/format-number %)}})}
                     (not= (vec labels) (vec ($ chart getLabels))) (assoc :valueRange nil)
                     themes (assoc :series themes))]
          ($ chart updateOptions 
             (clj->js opts))))
      :component-will-unmount (fn [this] ;;remove all listeners, give dygraphs a chance to clear up memory
                                (ev/unlistenByKey @watcher) (println "destroying line chart")
                                ($ (:chart (reagent/state this)) destroy))
      :component-did-mount
      (fn [this]
        (reset! watcher (ev/listen js/window "resize" #(resize-fn this)))
        (resize-fn this)        
        (let [dom (reagent/dom-node this)
              element (aget (.getElementsByClassName dom "lineChart") 0)
              legend (aget (.getElementsByClassName dom "dygraph-legend") 0)
              chart (js/Dygraph. element
                                 (clj->js data)
                                 (clj->js {:labels (cons (i18n ["Zeit"]) labels)
                                           :customBars (sequential? (second (first data)))
                                           :stepPlot true ; steps more representative of the nature of our 10min data
                                           :avoidMinZero true ; not necessarily include 0 in the y axis
                                           :dateWindow (date-window zoom)
                                           :connectSeparatedPoints true
                                           :axisLabelWidth 100
                                           :valueFormatter (vf-gen units scales)
                                           :zoomCallback (cb this)
                                           :interactionModel interaction
                                           :labelsDivWidth 450
                                           :labelsSeparateLines true
                                           :labelsUTC true
                                           :legend "always"
                                           :labelsDiv legend
                                           :height 700
                                           :highlightSeriesOpts {:strokeWidth 2 :strokeBorderWidth 1}
                                           :highlightSeriesBackgroundAlpha (if (>= (count labels) 4) 0.3 0.5)
                                           :colors chart-colors
                                           :series themes
                                           :underlayCallback (highlights-draw-fn highlights)
                                           ;:clickCallback points-clicked ; TODO annotation of sensors
                                           ;:pointClickCallback point-clicked ; TODO annotation of sensors
                                           :axes (if (scaled? scales)
                                                   {:y {:axisLabelFormatter (constantly "")}}
                                                   {:y {:axisLabelFormatter #(f/format-number %)}})}))]
          ($! chart -panCallback (cb this))
          (reagent/replace-state this {:chart chart})
          ;; canvas elements need to have a "tabindex" attribute to allow them to have focus
          ;; since Dygraph does not have any configuration for this, we add them after they were added to the DOM
          (timer/callOnce #(let [canvas-arr (.getElementsByTagName dom "canvas")
                                 len (.-length canvas-arr)]
                             (doseq [[i canvas] (map vector (range len 0 -1) (seq canvas-arr))]
                               (.setAttribute canvas "tabindex" i)))
                          500)))
      :reagent-render (fn [_ {lbls :labels maps-like? :maps-like?}]
                        [:div
                         [:div.lineChart {:style {:cursor (if maps-like? "pointer" "default")}}
                          "Chart goes here"]
                         [:div.dygraph-legend {:style {:min-height (* (inc (count lbls)) 20)}}]])})))

(defonce pan-handling
  #_"Replace builtin pan handling of Dygraphs, calls a zoom callback if present."
  (let [orig-handler js/Dygraph.Interaction.endPan]
    ($! js/Dygraph -Interaction.endPan
        (fn [event g context]
          (orig-handler event g context)
          ($! context -isPanning false)
          (when ($ g -panCallback)
            (let [x-range($ g xAxisRange)]
              ($ g panCallback (aget x-range 0) (aget x-range 1) ($ g yAxisRanges))))))
    ($! js/Dygraph -endPan ($ js/Dygraph -Interaction.endPan))))


(defn- zoom-callback [min-date max-date y-ranges zoomed?]
  (dispatch [:charts/zoom (tc/to-date-time min-date) (tc/to-date-time max-date) zoomed?]))

(defn remote-line-chart [] 
  (let [zoom (subscribe [:charts/time-interval :zoom])
        flags (subscribe [:charts/flags])
        labels (subscribe [:charts/labels])
        units (subscribe [:charts/units])
        themes (subscribe [:charts/themes])                       
        highlights (subscribe [:charts/highlights])
        chart-data (subscribe [:charts/scaled-dygraph-values])
        request (subscribe [:charts/new-request])]    
    (reagent/create-class
     {:component-did-mount (fn [this] ;; watch for changes to our request data, run initial request
                             (let [watch-key (random-uuid)]
                               (reagent/replace-state this {:watch-key watch-key})
                               (add-watch request watch-key (fn [_ _ old new] (fetch-data! old new)))
                               (fetch-data! nil @request)))
      :component-will-unmount (fn [this]
                                (let [watch-key (:watch-key (reagent/state this))]
                                  (remove-watch request watch-key)))
      :reagent-render (fn []
                        @request ;; touch once in rendering fn so it gets disposed correctly
                        (let [{:keys [maps-like?]} @flags 
                              {:keys [values scales]} @chart-data]
                          [:div.col-md-12
                           (when (not-empty @labels)
                             [line-chart values {:labels @labels
                                                 :units @units
                                                 :scales scales
                                                 :highlights @highlights
                                                 :themes @themes
                                                 :zoom @zoom
                                                 :zoom-callback zoom-callback
                                                 :maps-like? maps-like?
                                                 :interaction (if maps-like? google-maps-interaction ($ js/Dygraph -Interaction.defaultModel))}])]))})))

(defn changes-table []
  (let [show-pls? (subscribe [:charts/show-pls?])]
    (fn []
      [:div
       [:div.checkbox
        [:label
         [:input {:type "checkbox"
                  :checked @show-pls?
                  :on-change #(dispatch [:charts/toggle-show-pls?])}]
         "PLSMB anzeigen"]]
       [table/changes-table ::changes [:charts/changes]]])))

(defn errors-table []
  [table/error-table ::errors [:hydrated-errors [:charts/filtered-errors]] {:per-page 10}])

(defn modes-table []
  [table/modes-table ::modes [:charts/modes]])


(reg-sub :charts/heatmap-request
   :<- [:selected-series]
   :<- [:charts/heatmap-extents]
   :<- [:charts/time-interval :overall]
   :<- [:charts/time-interval :zoom]
   :<- [:charts/flags]
   (fn [[series extents overall zoom flags]]
     {:series (set series)
      :overall overall
      :zoom zoom
      :only-production? (:only-production? flags)
      :extents extents}))

(defn- fetch-heatmap! [old-req new-req]
  (let [ks [:series :overall :zoom :only-production?]
        [removed added] (diff (select-keys old-req ks) (select-keys new-req ks))
        changed (first (keys added))
        op? (:only-production? new-req)
        op-changed? (= :only-production? changed)
        sensor-series-old (take 2 (filter #(= :sensor (:type %)) (:series old-req)))
        sensor-series (take 2 (filter #(= :sensor (:type %)) (:series new-req)))
        new-heatmap-series? (not= sensor-series-old sensor-series)
        show? (= 2 (count sensor-series))
        {zs :start ze :end} (:zoom new-req)
        {zs' :start ze' :end} (:zoom added)
        {os :start oe :end} (:overall new-req)
        {os' :start oe' :end} (:overall added)         
        fetch-heatmap (fn [target params] ;; TODO allow the user to explicitely choose series to compare in heatmaps
                        (let [overall? (= target :overall)
                              params' (merge {:only-production? op?
                                              :series sensor-series
                                              :extents (when (not overall?) (:extents new-req))}
                                             params)]
                          (when overall?  (dispatch [:charts/received-heatmap :zoom nil]))
                          (remote/run-request {:url "/db/heatmap"
                                               :method :post
                                               :success [:charts/received-heatmap target]
                                               :params params'})))]
    ;; overall
    (cond
      (not show?) (dispatch [:charts/received-heatmap :overall nil])
      (or new-heatmap-series? os' oe' op-changed?)  (fetch-heatmap :overall {:start os :end oe}))
    ;; zoom
    (when (and zs ze)
      (cond
        (not show?) (dispatch [:charts/received-heatmap :zoom nil])
        (or new-heatmap-series? zs' ze' op-changed?) (fetch-heatmap :zoom {:start zs :end ze})))))


(defn remote-heatmap []
  (let [heatmap-data (subscribe [:charts/heatmap])
        labels (subscribe [:charts/labels])
        flags (subscribe [:charts/flags])
        dom-node (reagent/atom nil)
        request (subscribe [:charts/heatmap-request])]    
    (reagent/create-class
     {:component-did-mount (fn [this] ;; watch for changes to our request data, run initial request
                             (let [watch-key (random-uuid)]
                               (reagent/replace-state this {:watch-key watch-key})
                               (add-watch request watch-key (fn [_ _ old new] (fetch-heatmap! old new)))
                               (fetch-heatmap! nil @request)
                               (reset! dom-node (reagent/dom-node this))))
      :component-will-unmount (fn [this]
                                (let [watch-key (:watch-key (reagent/state this))]
                                  (remove-watch request watch-key)))
      :reagent-render
      (fn [] @pc/window-width @request ;; deref subscriptions so we get notified upon changes, cleared up upon unmount
        (let [{:keys [log? grey? inverse?]} @flags
              {:keys [data extents x-label y-label]} @heatmap-data
              {:keys [grey? log? inverse?]} @flags
              width (if-let [node @dom-node]
                      (.-clientWidth @dom-node)
                      800)]
          [:div
           (when (not-empty data)
             [hm/heatmap data extents
              {:width width
               :height 600
               :colors hm/inferno
               :log? log? :grey? grey? :inverse? inverse? :axis? true
               :x-label x-label :y-label y-label}])]))})))
