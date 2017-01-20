(ns scada-ui.algorithms
  (:require binning
            [clojure.core.reducers :as r]
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [clj-time.periodic :as tp]
            [clj-time.format :as tf]
            [streaming.piecewise :as pla]
            [streaming.protocols :as sp]
            [clojure.string :as str]
            [scada-ui.data.db :as db]
            [scada-ui.lasso :as lasso]
            [streaming.fading :as fade]            
            [scada-ui.data.structure :as struc]
            [chart-utils.jfreechart :as cjf]
            [timeseries.features :as feat]
            [org.clojars.smee
             [map :refer [map-values transform-map]]]))

(defrecord ThresholdValidator [minimums maximums min-counts max-counts since last-update]
  sp/NamedInputs
  (input-names [_] (concat (keys minimums) (keys maximums)))
  sp/StreamProcessor
  (update [this inputs]
    (assoc this
           :last-update (get inputs "timestamp")
           :min-counts (transform-map min-counts [k c]
                         (if (< (get inputs k) (get minimums k))
                           (inc c)
                           c))
           :max-counts (transform-map max-counts [k c]
                         (if (> (get inputs k) (get maximums k))
                           (inc c)
                           c))))
  (value [this]
    (let [mins (remove (fn [[k n]] (zero? n)) min-counts)
          maxs (remove (fn [[k n]] (zero? n)) max-counts)]
      (when-not (and (empty? mins) (empty? maxs))
        {:too-low (into {} mins)
         :too-high (into {} maxs)})))
  sp/TriggeredProcessor
  (run-trigger [this]
    (let [res (sp/value this)
          since (sp/add-durations last-update (distinct (concat (keys (:too-low res)) (keys (:too-high res)))) since)]
      {:state (assoc this
                     :since since
                     :min-counts (map-values (constantly 0) min-counts)
                     :max-counts (map-values (constantly 0) max-counts))
       :results (when res (assoc res :since since))})))

(defn thresholds-validator
  [limits]
  {:pre [(map? limits)
         (every? #(and (every? number? (vals %))
                       (every? #{:min :max} (keys %)))
                 (vals limits))]}
  (let [mins (reduce-kv (fn [res k {:keys [min]}]
                          (if min
                            (assoc res (feat/istr k "/min") min)
                            res))
                        {} limits)
        maxs (reduce-kv (fn [res k {:keys [max]}]
                          (if max
                            (assoc res (feat/istr k "/max") max)
                            res))
                        {} limits)]
    (map->ThresholdValidator
     {:minimums mins
      :maximums maxs
      :min-counts (map-values (constantly 0) mins)
      :max-counts (map-values (constantly 0) maxs)
      :last-update nil
      :since {}})))

(defn threshold-validator->limits
"`threshold-validator` and this function are opposites."
  [{:keys [minimums maximums]}]
  (reduce (fn [m [k v]]
            (let [[_ sensor suffix] (re-find #"(.*)/(.*)" k)]
              (assoc-in m [sensor (keyword suffix)] v)))
          {} (concat minimums maximums)))

(defrecord TrendEstimator [series-name lrc n]
  sp/NamedInputs
  (input-names [_] [series-name])
  sp/StreamProcessor
  (update [this m]
    (let [n (inc n)]
      (-> this
          (update :lrc sp/update n (get m series-name))
          (assoc :n n))))
  (value [_]
    ;; alert threshold might be defined via |slope| ~ max. percentage change per time interval
    ;; for example: alert if trend> +20%/month, @10min intervals this means slope > 0.2/(24*6*30)
    ;; problem: this formula overestimates the needed threshold, need to think about that
    ;; observation: better rule might be: alert if increase>x% in y days
    (sp/value lrc)))

(defn trend-estimator [series-name alpha confidence]
  {:pre [(contains? fade/t-distribution confidence)]}
  (TrendEstimator. series-name (fade/linear-regression-with-confidence alpha confidence) 0))

(defn- range-difference [[l u] x] 
  (cond
   (< x l) (- l x)
   (> x u) (- u x)
   :else 0))

(defn power-deviation-too-big-fn [threshold]
  (fn [power estimated-power-range]
    ;(<= (- (first estimated-power-range) power) threshold)
    (<= (range-difference estimated-power-range power) threshold)))

(defn rectangular-bin
"Like `binning/bounded-bin-fn` but for multiple dimensions. Expects a map
`{dimension-name {:min number :max number :step number}}`"
  [dimensions]
  {:pre [(map? dimensions)
         (every? map? (vals dimensions))]}
  (let [fns (into {}
                  (for [[k {mi :min ma :max width :width}] dimensions]
                    [k (binning/bounded-bin-fn mi ma width)]))]
    (fn [vs]
      (persistent!
       (reduce-kv (fn [res k f] (assoc! res k (f (get vs k)))) (transient {}) fns)))))

(defn wind-speed [name]
  (fn [vs] (get vs name)))

(defn air-density [pressure temperature]
  (* 1.225 (/ 288.15 (+ 273.15 temperature)) (/ pressure 1013.3)))

(comment
  (let [t (range -20 35)
        c (doto (incanter.charts/xy-plot t t :series-label "Air density kg/m³")
            (incanter.core/view))]
    (cjf/sliders [pressure (range 970 1020 10)]
       (let [densities (mapv #(air-density pressure %) t)]
         (cjf/perf-set-data c [t densities] 0))))
  )

(defn add-turbulence-intensity [inputs wind-avg-name wind-sd-name]
  (if wind-sd-name
    (let [x (get inputs wind-avg-name)
          ti (if (zero? x) 0.0 (/ (get inputs wind-sd-name 0.0) x))]
      (assoc inputs ::turbulence-intensity ti))
    inputs))

(defn add-air-density [inputs pressure-name temperature-name]
  (if (and pressure-name temperature-name)
    (let [pressure (get inputs pressure-name)
          temperature (get inputs temperature-name)]
      (assoc inputs ::density (air-density pressure temperature)))
    inputs))

(defn add-target [inputs target-names]
  (assoc inputs ::target (reduce min (map inputs target-names))))

(defrecord WindPowerCurveEstimator [wind-avg-name
                                    wind-sd-name
                                    power-name
                                    pressure-name
                                    temperature-name
                                    target-names
                                    names
                                    pla-learner
                                    options] 
  sp/NamedInputs
  (input-names [_] (concat names target-names (keep identity [wind-avg-name wind-sd-name power-name pressure-name temperature-name])))
  sp/StreamProcessor
  (update [this inputs]
    (let [now (get inputs "timestamp")
          learn-end (:learn-end options)]      
      (if (or (not learn-end)
              (time/before? now learn-end))
        (let [y (get inputs power-name)          
              inputs' (-> inputs
                          (add-target target-names)
                          (add-turbulence-intensity wind-avg-name wind-sd-name)
                          (add-air-density pressure-name temperature-name))]
          (update this :pla-learner #(sp/update % inputs' y)))
        this)))
  sp/Predictor
  (predict [_ inputs]
    (let [inputs' (-> inputs
                      (add-target target-names)
                      (add-turbulence-intensity wind-avg-name wind-sd-name)
                      (add-air-density pressure-name temperature-name))]
      (let [[low high :as pred] (sp/predict pla-learner inputs')]
        (if (or (Double/isNaN ^double low)
                (Double/isNaN ^double high))
          [(inputs power-name) (inputs power-name)]
          pred))))  
  sp/Serializable
  (hydrate [this]
    (let [pla (pla/serializable-fplr
               {:alpha (:alpha options) 
                :bin-fn {:function ::rectangular-bin
                         :arguments [(:dimensions options)]}
                :x-fn {:function ::wind-speed
                       :arguments [wind-avg-name]}
                :confidence (:confidence options)
                :should-learn? {:function ::power-deviation-too-big-fn
                                :arguments [(:threshold options)]}})]
      (assoc this :pla-learner (sp/hydrate pla))))
  (dehydrate [this]
    (update this :pla-learner sp/dehydrate)))

(defrecord WindpowerLosses [wpce losses-min losses-avg sum
                            last-update interval-hours]
  sp/NamedInputs
  (input-names [_] (sp/input-names wpce))
  sp/StreamProcessor
  (update [this inputs]
   (let [power (get inputs (:power-name wpce))
         now (get inputs "timestamp")
         last-update (or last-update now)
         wpce' (sp/update wpce inputs)
         [low high] (sp/predict wpce inputs)
         pred-avg (+ low (* 0.5 (- high low)))
         losses-min' (+ losses-min (* interval-hours (max 0 (- low power))))
         losses-avg' (+ losses-avg (if (< power low) (* interval-hours (max 0 (- pred-avg power))) 0))
         sum' (+ sum (* interval-hours power))]
     (WindpowerLosses. wpce' losses-min' losses-avg' sum' now interval-hours)))
  sp/Predictor
  (predict [_ inputs]
    (sp/predict wpce inputs))
  sp/TriggeredProcessor
  (run-trigger [this]
    {:state (assoc this :losses-min 0.0 :losses-avg 0.0 :sum 0.0)
     :results (when (> losses-min 0)
                {:losses-min losses-min ;; in kW/h
                 :losses-avg losses-avg ;; in kW/h
                 :sum sum})}))


(defn estimate-power-curve-with-turbulences
  [{:keys [wind wind-sd power pressure temperature targets]}
   {:keys [alpha threshold confidence dimensions learn-end max-power]
    :as opts
    :or {alpha 1.0
         confidence 0.95
         threshold java.lang.Double/MAX_VALUE
         targets []
         dimensions {}
         max-power 5000}}]
  (-> {:wind-avg-name wind
       :wind-sd-name wind-sd
       :power-name power
       :pressure-name pressure
       :temperature-name temperature
       :target-names targets
       :names (keys dimensions)
       ;; if turbulence index>0.2, the variance is soo high, the linear regression might give extreme predictions, which then will not be corrected that quickly.
       :options {:dimensions (cond-> dimensions
                               wind-sd (assoc ::turbulence-intensity {:min 0.1 :max 0.2 :width 0.02})
                               (and pressure temperature) (assoc ::density {:min 1.1 :max 1.3 :width 0.05})
                               (not-empty targets) (assoc ::target {:min 0.0 :max max-power :width (/ max-power 50)}))
                 :alpha alpha
                 :confidence confidence
                 :threshold threshold
                 :learn-end learn-end}}
      map->WindPowerCurveEstimator
      sp/hydrate))


(defn estimate-power-curve
"For external use."
  [conn plant from to
   {:as opts
    :keys [confidence threshold alpha dimensions learn-end
           wind-sensor power-sensor target-sensors]
    :or {confidence 0.999
         threshold 100
         alpha (fade/half-life->alpha (* 24 6 7))
         dimensions {}
         wind-sensor (struc/wind-speed-sensor-name plant)
         power-sensor (struc/power-sensor-name plant)
         target-sensors (struc/target-sensor-names plant)}}]
  (let [opts {:confidence confidence :threshold threshold :alpha alpha}
        wind-key (str wind-sensor "/avg")
        power-key (str power-sensor "/avg")
        target-keys (map #(str % "/avg") target-sensors)
        names (cond-> {:wind wind-key
                       :power power-key
                       :targets target-keys}
                (contains? (struc/sensor-type plant wind-sensor) :std) (assoc :wind-sd (str wind-sensor "/std"))                
                ;; TODO :pressure "14/6/avg"
                 ;; TODO :temperature "14/5/avg"
                )
        dimensions (assoc dimensions wind-key {:min 0 :max 20 :width 1.0})
        learner (estimate-power-curve-with-turbulences names (assoc opts :dimensions dimensions :learn-end learn-end :max-power (struc/max-power plant)))]
    (second
     (db/process-raw-values-in-time-range conn plant (sp/input-names learner) from to true
       (fn [rows]
         (reduce (fn [[learner estimations] row]
                   (let [l' (sp/update learner row)
                         [mi ma] (sp/predict learner row)
                         power (get row power-key)]
                     [l' (conj estimations [(get row "timestamp") [mi power ma]])]))
                 [learner []] rows))))))

;;;;;;;;;;;;; run anomaly detection algorithms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nop ([]) ([_]) ([_ _]))

(def fmt (partial tf/unparse (tf/formatters :basic-date)))

(defrecord PersistingMultiProcessor [by-id timestamp-key options conn last-update persisted?]
  sp/NamedInputs
  (input-names [_]
    (->> by-id vals (mapcat (comp (partial mapcat feat/find-stems) sp/input-names)) set))
  sp/StreamProcessor
  (value [this] this)
  (update [this inputs]
    (let [now (get inputs timestamp-key)
          ;; persist results for each new day
          update? (and now last-update (time/after? now last-update))
          triggered? (and update? (not= (time/day now) (time/day last-update)))
          by-id' (if triggered?
                   (:state (sp/run-trigger this))
                   by-id)]
      (assoc this
             :last-update (if update? now last-update)
             :persisted? (if update? triggered? persisted?)
             :by-id (transform-map by-id' [id {last-ts :last-update :as state}]
                      (if (or (not last-ts) (time/after? now last-ts))
                        (assoc (sp/update state inputs) :last-update now)
                        state)))))
  sp/TriggeredProcessor
  (run-trigger [this]
    (if persisted? ;; do not rerun trigger code if called twice in a row
      {:state by-id}
      (let [m (transform-map by-id [id nested] 
                (if (satisfies? sp/TriggeredProcessor nested)
                  (let [res (sp/run-trigger nested)]
                    (when (not-empty (:results res))
                      (db/store-algorithm-result! conn id last-update (:results res)))
                    res)
                  {:state nested}))]
        {:results (transform-map m [_ r] (:results r))
         :state (transform-map m [_ r] (:state r))}))))

(defn- merge-opts
"If we have multiple models that should get applied to a single sequence of rows, 
we need to merge the options relevant for `derive-and-process-rows-fn`."
  [opts]
  (if (map? opts)
    opts
    {:max-lag (reduce max 0 (:max-lag opts))
     :add (distinct (mapcat :add opts))
     :non-linear? (boolean (some :non-linear? opts))
     :half-lifes (->> opts (mapcat :half-lifes) distinct sort)}))

(defn load-algorithms [conn algs plant]
  (let [states (->>
                (for [{id :id end :learn-end alg :algorithm} algs
                      :let [{:keys [state last-update]} (db/algorithm-state conn id)
                            last-update (or last-update end)
                            state (or state alg)]]
                  [id (assoc state :last-update last-update)])
                (into {}))
        opts (merge-opts (map :options (vals states)))
        earliest (time/earliest (map :last-update (vals states)))]
    (map->PersistingMultiProcessor {:by-id states
                                    :timestamp-key "timestamp"
                                    :conn conn
                                    :options opts
                                    :last-update earliest
                                    :persisted? true})))

;; FIXME since our derived features might be stateful, we need to store them!
(defn run-algorithms!
  ([conn] (run-algorithms! conn nil))
  ([conn type]
   (->> (db/plants-with-algorithms conn type)
        ;;(r/filter #(= (:plant %) "GT42"))
        (r/map (fn [{:keys [plant]}]
                 (let [algs (db/algorithms-by conn {:plant plant :type type})
                       multi (load-algorithms conn algs plant)
                       last-date (:last-update multi)
                       _ (do (printf "model ids=%s for plant %s, dates: %s - %s\n"
                                     (-> multi :by-id keys vec str)
                                     plant
                                     (fmt last-date)
                                     (fmt (time/today-at-midnight)))
                             (flush))                                                
                       opts (:options multi)
                       columns (sp/input-names multi)
                       multi' (lasso/apply-models conn
                                                  columns
                                                  sp/update
                                                  multi
                                                  plant
                                                  (time/minus last-date (time/days 1))
                                                  (time/with-time-at-start-of-day (time/now))
                                                  opts)
                       last-update (:last-update multi')]
                   (when (not= last-date last-update)
                     ;; since we trigger detailed results when passing midnight,
                     ;; we might not have triggered for the last complete day
                     (when (not (:persisted? multi'))
                       (sp/run-trigger multi'))
                     ;; store algorithms states
                     (doseq [[id state] (:by-id multi')]
                       (db/store-algorithm-state! conn plant id last-update state)))
                   (println "done processing models for" plant))))
        (r/fold 1 nop nop))))




(comment
  (require 'wind.visualizations)
  (require '[chart-utils.jfreechart :as cjf])
  (require '[scada-ui.data.db :as db])
  (declare learner)
  (let [;power-key "gridproductionpoweravg125/avg"
        power-key "2/7/avg" ;; ABB-LS Active Power
        ;power-key "10/43/avg" ;; Drehzahl Rotor 1
        ;wind-key "ambientwindspeedavg74/avg"
        wind-key "14/12/avg" ;; Windgeschwindigkeit 1+2
        ;wind-key "14/15/avg" ;; Windgeschwindigkeit Rotor
        ;wind-sd-key "ambientwindspeedavg74/std"
        wind-sd-key "14/12/std"
        ;plant "E1"
        plant "GT61"
        start (time/date-time 2016 1 1)
        end (time/date-time 2016 12 5)
        learn? true
        conn scada-ui.handler/conn
        learner (if learn?
                  (estimate-power-curve-with-turbulences
                   {:wind wind-key
                    :wind-sd wind-sd-key
                    :power power-key
                    :targets ["_reduction/avg" "1/35/avg" "15/14/avg"]
                    :pressure "14/6/avg"
                    :temperature "14/5/avg"
                    }
                   {:confidence 0.999
                    :threshold 100
                    :alpha (fade/half-life->alpha (* 24 6 10))
                    :max-power 5000
                    :learn-end (time/date-time 2016 12 1)
                    :dimensions {wind-key {:min 0 :max 20 :width 1.0}}})
                    learner)
        learner (map->WindpowerLosses {:wpce learner
                                       :losses-min 0.0
                                       :losses-avg 0.0
                                       :sum 0.0
                                       :last-update start
                                       :interval-hours (-> plant struc/plant-spec-of :sampling-rate (/ 3600.0))})
        series (sp/input-names learner)
        [learner estimations] (time
                               (db/process-raw-values-in-time-range conn plant series start end true
                                  (fn [rows]
                                    (reduce (fn [[learner estimations] row]
                                              (let [est (sp/predict learner row)
                                                    now (get row "timestamp")                                                   
                                                    last-update (:last-update learner)
                                                    trigger? (and now last-update (time/after? now last-update) (not= (time/day now) (time/day last-update)))
                                                    {learner :state res :results} (if trigger?
                                                                                    (sp/run-trigger learner)
                                                                                    {:state learner})]
                                                (when res
                                                  (printf "%s: sum=%10.2f min=%8.2f (%4.2f%%), avg=%8.2f (%4.2f%%)\n"
                                                          (fmt now) (:sum res)
                                                          (:losses-min res)
                                                          (* 100 (/ (:losses-min res) (:sum res)))
                                                          (:losses-avg res)
                                                          (* 100 (/ (:losses-avg res) (:sum res)))))
                                                [(sp/update learner row)
                                                 (conj estimations est)]))
                                            [learner []] rows))))]    
    (def learner learner)
    (def estimations estimations)        
    (let [data (db/raw-value-in-time-range conn plant series start end true)
          wind-data (into [] (get data wind-key))
          power-data (into [] (get data power-key))
          wind-sd-data (into [] (get data wind-sd-key))
          {:keys [bin-fn state]} (:pla-learner learner)
          timestamps (mapv tc/to-long (get data "timestamp"))
          chart (wind.visualizations/visualize-wind-power-curve-estimation
                 timestamps
                 wind-data
                 power-data
                 estimations
                 {:title plant
                  :wind-deviations wind-sd-data
                  :alpha (fade/half-life->alpha (* 24 6))
                  :ignore-spikes? true})]
      #_(wind.visualizations/plot-sliding-heatmap
       wind-data power-data timestamps plant
       {:points-per-day (* 6 24)
        :days-in-window 7
        :overall? true
        :other-chart chart})
      ;; histogram of turbulence indices
      #_(incanter.core/view (incanter.charts/histogram (map #(if (zero? %1) 0.0 (/ %2 %1)) wind-data wind-sd-data) :nbins 100))
      #_(wind.visualizations/plot-estimated-power-curve (:pla-learner learner) wind-key wind-sd-key)
      ))

  
  ;; interactive experiments with trends estimation
  (let [;sensor "10/40/avg" plant "GT61"; Temperatur Rotorlager 1, GT61
        ;sensor "1/29/avg" plant "GT42"; wasserleitfaehigkeit, GT42
        ;sensor "14/5/avg" plant "GT61"; außentemperatur gondel
        sensor "11/10/avg" plant "GT08"; Kuehlwasserdruck 1
       ; sensor "10/42/avg" plant "GT60"
        start (time/date-time 2015 12 1)
        end (time/date-time 2017)
        fetch (fn [days confidence]
                (let [alpha (fade/half-life->alpha (* 24 6 days))
                      trend-est (trend-estimator sensor alpha confidence)
                      thresh-val (thresholds-validator {"11/10" {:min 2.5 :max 10}})                      
                      columns (concat (sp/input-names trend-est) (sp/input-names thresh-val))
                      [[trend-est' thresh-val'] estimations]
                      (db/process-raw-values-in-time-range scada-ui.handler/conn plant columns start end true
                         (fn [rows]
                           (reduce (fn [[[trend thres] estimations] row]
                                     (let [trend' (sp/update trend row)
                                           thres' (sp/update thres row)
                                           {:keys [slope slope-confidence]} (sp/value trend')
                                           any-nan? (or (Double/isNaN slope) (Double/isNaN (first slope-confidence)))]
                                       [[trend' thres'] (if any-nan?
                                                          estimations
                                                          (conj estimations {:timestamp (tc/to-long (get row "timestamp")) :min (first slope-confidence) :avg slope :max (second slope-confidence) :value (get row sensor)}))]))
                                   [[trend-est thresh-val] []] rows)))]
                  (println thresh-val')
                  (drop (* days 144) estimations)))
        chart (doto (cjf/deviation-plot [] [] [] [] :title "Trend" :y-label "Absolute changes per selected time interval")
                (cjf/use-time-axis)
                (cjf/add-value-marker 0 "constant, no trend")
                (incanter.core/view))
        #_#_gradients-chart (doto (incanter.charts/time-series-plot [] [] :title "Gradients" :y-label "dx/dt per time step")
                          (cjf/set-discontinuous)
                          (cjf/add-value-marker 0 "constant")
                          incanter.core/view)]
    (cjf/sliders
     [days [30 0.5 1 2 3 4 7 10 20 30 45 60 90 180]
      confidence (reverse (sort (keys fade/t-distribution)))]
     (let [trends (fetch days confidence)
           avgs (map :avg trends)
           ;gradients (cons 0 (map (fn [[a b]] (- b a)) (partition 2 1 avgs)))
           factor (* days 24 6)
           ts (map :timestamp trends)
           ;factor 1.0
           ;; use values as absolute changes per `days` interval
           tuples (map (fn [{:keys [timestamp avg min max]}]
                         [timestamp (* factor avg) (* factor min) (* factor max)]) trends)]
       (cjf/perf-set-data chart tuples 0)
       #_(cjf/perf-set-data gradients-chart [ts gradients] 0))))
  
  )
