(ns scada-ui.lasso
  (:require [timeseries.lasso :as lasso]
            [org.clojars.smee.map :refer [map-values transform-map]]
            [timeseries.features :as feat]
            [timeseries.functions :refer [normalizing-constants]]
            [scada-ui.util.levensthein :refer [levenshtein-distance]]
            [scada-ui.util :refer [kw-name]]
            [scada-ui.data.db :as db]
            [scada-ui.data.structure :as struc]
            [clj-time.core :as time]
            [streaming.protocols :as sp]
            [streaming.fading :as fade]
            [clojure.core.reducers :as r]
            [clojure.string :as str]))


;;;;;;;;;;;;;; learning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn topmost-predictors [model n]
  (->> model :parameters :attributes (sort-by (comp :importance second)) reverse (map (comp feat/find-stem first)) (take n)))

(defn- learn-lasso-models*
"Learn LASSO models for as many input columns as possible."
  [features target-names {:keys [unwanted? models-per-target thresholds debug? topmost]
                         :as opts
                         :or {unwanted? (constantly false)
                              models-per-target 1
                              topmost 2
                              minimum-quality 0.9
                              map-fn map
                              thresholds {:r-squared 0.9}}}]
  (let [minimum-quality (:r-squared thresholds)
        features (-> features
                     (->> (map-values lasso/as-array)
                          (into (sorted-map))))
        feature-names (vec (keys features))
        ts (lasso/lasso-regression-dataset features (first target-names))
        index-of (into {} (map-indexed #(vector %2 %1) feature-names))]
    (->> target-names
         vec
         (r/map (fn [target-name]
                  (let [target-idx (index-of target-name)] (when (nil? target-idx) (println "ERROR: could not find data of" target-name))
                    (loop [n 0, res [], unwanted #{target-name}]
                      (let [unwanted-indices (->> feature-names
                                                  (filter #(or (some (fn [s] (str/starts-with? % s)) unwanted) (unwanted? target-name %)))
                                                  (mapv index-of))
                            ts (lasso/derive-regression-dataset ts target-idx unwanted-indices)
                            model (lasso/lasso-regression ts feature-names opts)
                            quality (-> model :parameters :r-squared)]
                        (when debug? (printf  "%d/%d %s: r²=%.5f |features|=%d\n" (inc n) models-per-target target-name quality (-> model :parameters :attributes count)))
                        (if (and (>= quality minimum-quality) (< n models-per-target))
                          (recur (inc n) (conj res [(str target-name "|v" (inc n)) model]) (into unwanted (topmost-predictors model topmost)))
                          (mapv (fn [[k m]] [(str k "/" n) m]) res)))))))
         (r/remove empty?)
         (r/fold 1 (fn ([] []) ([a b] (into a b))) into)
         (into {}))))

(defn longest-constant-duration
  "To decide whether a constant subsequence is out of the ordinary it
is useful to know, if long stretches of constant values are normal or not.
This functions returns the proportion of the aggregated length of all
constant subsequences of `vs`."
  [values min-length]
  (loop [[v & vs] values, last-value nil, n 0, sum 0]
    (cond
      (nil? v) (double (/ (if (> n min-length) (+ sum n) sum) (count values)))
      (= v last-value) (recur vs last-value (inc n) sum)
      :else (recur vs v 0 (if (> n min-length) (+ sum n) sum)))))

(defn learn-lasso-models  
  [features targets opts]
  (let [unwanted-predictor? (set (get-in opts [:remove :predictors]))
        to-remove (->> features
                       keys
                       (filter (fn [k] (let [vs (get features k)]
                                        (or
                                         (unwanted-predictor? k)
                                         (unwanted-predictor? (feat/find-stem k))
                                         ;; remove constant series
                                         (every? #{(first vs)} vs)))))
                       ((juxt identity (partial mapcat feat/find-stems)))
                       (apply concat)
                       set)
        features (reduce-kv (fn [m k v] (if (or (to-remove k) (some to-remove (feat/find-stems k)))
                                         m
                                         (assoc m k v))) {} features)
        targets (vec (remove to-remove targets))
        min-len (-> opts :thresholds :constant-min-length)
        constants-of (->> features
                          (r/map (fn [name vs]
                                   [name (assoc (normalizing-constants vs)
                                                :constant (longest-constant-duration vs min-len))]))
                          (r/fold merge))
        edit-distance (:max-edit-distance opts)
        distance (memoize levenshtein-distance) ;; quite costly, cache it
        name->label (:name->label opts)
        unwanted-fn (fn [target ^String candidate]
                      ;; we don't want to rely on derived features of the target feature
                      (or (.startsWith candidate target)
                          ;; only learn from past values
                                        ;(not (.contains candidate "-lag-"))
                          ;; exclude series which are too similar to each other
                          (and (> edit-distance 0)
                               (some (fn [c]
                                       (<= (distance (name->label target) (name->label c))
                                           edit-distance)) (feat/find-stems candidate)))))
        target-names (->> targets
                          (remove (set (get-in opts [:remove :targets])))
                          sort)
        _ (do (printf "learning up to %d models from %d features\n" (* (or (:models-per-target opts) 0) (count target-names)) (count features)) (flush))
        models (learn-lasso-models* features target-names (assoc opts :unwanted? unwanted-fn))]
    (println "learned" (count models) "models")
    (lasso/initialize-multi-lasso models constants-of (assoc opts :timestamp-key "timestamp"))))

;;;;;;;;;; apply learned LASSO models to a data range ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- derive-and-process-rows-fn
  ""
  [sampling-rate
   {:keys [max-lag add non-linear? half-lifes] :or {max-lag 2, add [], half-lifes []}}
   initial-state
   processor-fn]  
  (let [;; if the model does not use the columns that may be summed, we skip those
        new-enhancer (fn [] (apply comp (remove nil? [(feat/row-wise-lagged max-lag)
                                                     (feat/row-wise-integral half-lifes)
                                                     (when non-linear? feat/row-wise-nonlinear)             
                                                     (partial feat/row-wise-added add)])))        
        ;; integral feature need history even within non-production times!, alternative: drop a long enought history so that we saw enough data in the new interval
        drop-count-init (max max-lag (if (empty? half-lifes)
                                       0
                                       (* 2 (apply max half-lifes))))]
    (fn [rows]
      ;; make sure we use a new lagged-fn when the gap between successive data points is larger than the sampling rate of this plant
      (loop [rows rows,
             state initial-state
             last-ts nil
             f (new-enhancer),
             drop-count drop-count-init]
        (let [{ts "timestamp" :as row} (first rows)
              seconds (if (nil? last-ts) 0 (time/in-seconds (time/interval last-ts ts)))
              new-interval? (or (nil? last-ts) #_(> seconds sampling-rate))
              f (if new-interval? (new-enhancer) f)]
          (cond
            (empty? rows) state
            new-interval? (recur rows state ts f drop-count-init)
            (> drop-count 0) (do ;; remember this row for later lagging and drop it, needed so each row has `max-lag` ancestors (feat/row-wise-lagged is stateful)
                               (-> row
                                   (dissoc "timestamp")
                                   f)
                               (recur (next rows) state ts f (dec drop-count)))
            ;; add non-linear and lagged features
            :else (recur (next rows) (processor-fn state  (-> row
                                                              (dissoc "timestamp")
                                                              f
                                                              (assoc "timestamp" ts))) ts f 0)))))))

(defn apply-models
  "Load measurement data from one table, apply LASSO models to each entry. 
  Returns {target {predictor-name {:residual fade/average, :correlations fade/correlation}} with correlations of predictors to residuals of target data and the model's prediction."
  [conn columns ingester initial-state plant start end opts]  
  (let [{sr :sampling-rate} (db/plant-spec-of conn plant)
        col-set (set columns)
        opts (update opts :add (fn [add] (vec (filter #(every? col-set %) add))))
        prod-col (struc/production-column plant)
        ingester' (fn [state row]
                    (if (= 1.0 (get row prod-col))
                      (ingester state row)
                      state))       
        f (derive-and-process-rows-fn sr opts initial-state ingester')]
    (db/process-raw-values-in-time-range conn plant (cons prod-col columns) start end false f)))


(defn zoom-to [models ^String search-term exact?]
  (let [filter-fn (if exact? (partial = search-term) (fn [^String k] (.contains k search-term)))
        model-keys (filter filter-fn (keys (:models models)))
        models (update-in models [:models] select-keys model-keys)
        names (sp/input-names models)
        columns (-> (sorted-set)
                    (into names) ;; only if timeseries.streaming.lasso changes to branch "improve_heuristics"
                    (into (mapcat feat/find-stems names))
                    (into (map feat/find-stem names)))
        filter-stats (fn [m] (into {} (for [[k v] m :when (columns k)] [k v])))]
    (-> models
        (update-in [:state :models] select-keys model-keys)
        (update-in [:statistics] filter-stats)
        (update-in [:state :statistics] filter-stats))))

(comment
  ;; problem: catastrophic cancelation
  (let [sx 1.78524E7
        sy -1339.7208748533042
        sxx 5.7841776E11
        syy 3257.4930855869966
        sxy -4.34069563452471E7
        n 551.0]
    (/ (- sxy (/ (* sx sy) n))
       (* (Math/sqrt (Math/abs (- sxx (/ (* sx sx) n)))) ;; rounding error :(
          (Math/sqrt (Math/abs (- syy (/ (* sy sy) n)))))))

  (let [n 549.0,
        sx 3.201768E9,
        sy -1334.6905017418896,
        sxx 1.8672710976E16,
        sxy -7.783915006158695E9,
        syy 3244.8204505058216
        ]
    (println (- sxy (/ (* sx sy) n)))
    (/ (- sxy (/ (* sx sy) n))
       (* (Math/sqrt (Math/abs (- sxx (Math/pow (/ sx n) 2)))) 
          (Math/sqrt (Math/abs (- syy (Math/pow (/ sy n) 2)))))))

  (let [n 688.0
        sx 0.0
        sy -1952.2032544844756
        sxx 0.0
        sxy 0.0
        syy 5539.408919522979

        nroot (Math/sqrt n)
        sx' (/ sx nroot)
        sy' (/ sy nroot)]
  (/ (- sxy (* sx' sy'))
     (* (Math/sqrt (Math/abs (- sxx (* sx' sx'))))
        (Math/sqrt (Math/abs (- syy (* sy' sy')))))))
 
 )
