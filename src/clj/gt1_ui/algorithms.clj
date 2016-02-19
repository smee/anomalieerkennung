(ns gt1-ui.algorithms
  (:require binning
            [clj-time.core :as time]
            [clj-time.coerce :as tc]
            [streaming.piecewise :as pla]
            [streaming.protocols :as sp]))


(defrecord ThresholdValidator [series-name upper lower]
  sp/AnomalyDetector
  (description [_] (format "Schwellwertüberwachung auf '%s' im Bereich %f bis %f" (str series-name) (double lower) (double upper)))
 
  (input-names [_] [series-name])
  (train [this _] this)
  (run [this inputs]
    (let [v (get inputs series-name)]
      (cond
        (< v lower) :too-low
        (> v upper) :too-high
        :else nil))))

(defn validate-thresholds
  [{:keys [lower upper series-name]
    :or {lower Double/NEGATIVE_INFINITY upper Double/POSITIVE_INFINITY}}]
  {:pre [(number? lower) (number? upper) (not (nil? series-name))]}
  (map->ThresholdValidator {:lower lower :upper upper :series-name series-name}))


(defn- range-difference [[l u] x] 
  (cond
   (< x l) (- l x)
   (> x u) (- u x)
   :else 0))

(defn power-deviation-too-big-fn [threshold]
  (fn [power estimated-power-range] 
    (<= (range-difference estimated-power-range power) threshold)))

;;TODO binning by windspeed, upper power limit set, turbulence intensity
(defrecord WindPowerCurveEstimator [x-name y-name pla-learner options] 
  sp/AnomalyDetector
  (description [_] (str "Leistungskennlinienrekonstruktion auf" x-name))
  (input-names [_] [x-name y-name])
  (train [this inputs]
    (let [x (get inputs x-name)
          y (get inputs y-name)]
      (update this :pla-learner #(% x y))))
  (run [_ inputs]
    (pla/predict (:bin-fn pla-learner) (:x-fn pla-learner) (:state pla-learner) (get inputs x-name)))
  
  sp/Serializable
  (hydrate [this]
    (let [pla (pla/serializable-fplr
               {:alpha (:alpha options) 
                :bin-fn {:function :binning/bounded-bin-fn
                         :arguments [(:wind-min options)
                                     (:wind-max options)
                                     (:wind-bin-width options)]}
                :x-fn {:function :clojure.core/identity}
                :confidence (:confidence options)
                :should-learn? {:function :gt1-ui.algorithms/power-deviation-too-big-fn
                                :arguments [(:threshold options)]}})]
      (assoc this :pla-learner (sp/hydrate pla))))
  (dehydrate [this]
    (update this :pla-learner sp/dehydrate)))



(defn estimate-power-curve [wind-series-name
                            power-series-name
                            {:keys [wind-min wind-max wind-bin-width alpha threshold confidence]
                             :as opts
                             :or {wind-min 0 
                                  wind-max 20
                                  wind-bin-width 1.0
                                  alpha 0.9999
                                  confidence 0.95
                                  threshold java.lang.Double/MAX_VALUE}}]
  (-> {:x-name wind-series-name
       :y-name power-series-name
       :options {:wind-min wind-min
                 :wind-max wind-max
                 :wind-bin-width wind-bin-width
                 :alpha alpha
                 :confidence confidence
                 :threshold threshold}}
      map->WindPowerCurveEstimator
      sp/hydrate))



(comment
  (let [val (validate-thresholds {:lower 2 :upper 4 :series-name :foo})]
    (println (sp/input-names val))
    (sp/run val {:foo 8})
    (println (sp/description val))
    (-> val sp/dehydrate pr-str read-string sp/hydrate))

  (require 'wind.visualizations)
  (let [power-key "1/2/7/avg" ;; Windgeschwindigkeit 1+2
        ;;power-key "2/10/43/avg" ;; Drehzahl Rotor 1
        wind-key "2/14/12/avg"
        wind-sd "2/14/12/sd"        
        plant "GT32"
        start (time/date-time 2015 6 22)
        end (time/date-time 2016 4 20)
conn gt1-ui.handler/conn
        time-intervals (gt1-ui.data.availability/operation-time-intervals conn plant start end)        
        data (gt1-ui.data.db/raw-value-in-time-range conn plant [power-key wind-key] time-intervals)
        wind-data (into [] (get data wind-key))
        power-data (into [] (get data power-key))]
    (loop [learner (estimate-power-curve wind-key power-key {:confidence 0.99 :wind-bin-width 0.5})
           wind wind-data
           power power-data]
      (if (or wind power)
        (recur (sp/train learner {wind-key (first wind,)
                               power-key (first power)})
               (next wind)
               (next power))
        (def learner learner)))
    (let [{:keys [bin-fn state]} (:pla-learner learner)
          estimations (mapv #(sp/run learner {wind-key %}) wind-data)]
      (wind.visualizations/plot-estimated-power-curve bin-fn state)
      (wind.visualizations/visualize-wind-power-curve-estimation
       (mapv tc/to-long (get data "timestamp"))
       wind-data
       power-data
       estimations
       bin-fn)))
  
  
  )
