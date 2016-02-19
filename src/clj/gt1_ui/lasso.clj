(ns gt1-ui.lasso
  (:require [timeseries.lasso :as lasso]
            [org.clojars.smee.map :refer [map-values]]
            [timeseries.features :as feat]
            [timeseries.functions :refer [normalizing-constants]]
            [gt1-ui.util.levensthein :refer [levenshtein-distance]]
            [gt1-ui.util :refer [kw-name]]
            [gt1-ui.data.db :as db]
            [clj-time.core :as time]
            [streaming.protocols :as sp]
            [streaming.fading :as fade]
            [clojure.core.reducers :as r]
            [clojure.string :as str]))

(defmacro transform-map
  "Reduce an associative datastructure into a new one. Assumes that `input` can be transient."
  {:style/indent 2}
  [input [k v] & body]
  `(let [input# ~input]
     (persistent!
      (reduce-kv (fn [m# ~k ~v]
                   (assoc! m# ~k ~@body))
                 (transient (empty input#))
                 input#))))

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
                  (let [target-idx (index-of target-name)]
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


(defn learn-lasso-models  
  [columns segment-lengths opts]
  (let [name->label (:name->label opts)
        names (keys columns)
        distance (memoize levenshtein-distance) ;; quite costly, cache it
        unwanted-predictor? (set (get-in opts [:remove :predictors]))
        combined-columns (reduce (fn [m names]
                                   (assoc m (str/join "+" names) (apply mapv + (vals (select-keys columns names))))) {} (:add opts))
        
        columns (->> (feat/add-added-columns columns (:add opts))                     
                     (remove (fn [[k vs]] (or
                                          (unwanted-predictor? k)
                                          ;; remove constant series
                                          (every? #{(first vs)} vs))))
                     (into {})) 
        features (cond-> columns
                   (:non-linear? opts) (merge (feat/derive-nonlinear-features columns))
                   true (feat/add-lagged-columns segment-lengths (:max-lag opts)))        
        constants-of (r/fold merge (r/map (fn [name vs] [name (normalizing-constants vs)]) features))
        edit-distance (:max-edit-distance opts)
        opts (assoc opts :unwanted? (fn [target ^String candidate]
                                      ;; we don't want to rely on derived features of the target feature
                                      (or (.startsWith candidate target)
                                          ;; only learn from past values
                                          ;(not (.contains candidate "-lag-"))
                                          ;; exclude series which are too similar to each other
                                          (and (> edit-distance 0)
                                               (some (fn [c]
                                                       (<= (distance (name->label target) (name->label c))
                                                           edit-distance)) (feat/find-stems candidate))))))
        target-names (->> columns
                          keys
                          (remove (set (get-in opts [:remove :targets])))
                          sort)
        _ (printf "learning up to %d models from %d features\n" (count target-names) (count features))
        models (learn-lasso-models* features target-names opts)]
    (println "learned" (count models) "models")
    (lasso/map->MultiLasso {:models models :statistics constants-of})))

;;;;;;;;;; apply learned LASSO models to a data range ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initial-state
"Create initial, empty state as parameter for `ingest-new-measurement`."
  [^timeseries.lasso.MultiLasso lasso-models {:keys [alpha alpha-short] :or {alpha 1.0 alpha-short 0.9}}]
  (let [c (fade/correlation alpha)
        v (fade/variance alpha)
        a (fade/average alpha)
        ;; target names may have a suffix, if there is more than one model per target sensor
        names (into (set (map feat/find-stem (keys (:models lasso-models)))) (mapcat sp/input-names (vals (:models lasso-models))))
        all-names (into (set names) (map feat/find-stem names))]
    {:statistics (zipmap all-names (repeat {:variance v :sd-ratio 1 :deviation 0 :average a}))
     :models (transform-map (:models lasso-models) [target model]
               (let [attr-names (sp/input-names model)]
                 {:predictors (zipmap attr-names (repeat c))
                  :residual (fade/average alpha)}))}))



(defn ingest-new-measurement
  "Takes a lasso model, a current reduction/state (see `initial-state` for its shape) and a row ({sensor-name value}).
Returns the new state 
{target-name {:residual average relative residual, 
              :correlations {predictor, correlation of residual with predictor}, 
              :correlations-without {predictor, {predictor' correlation of predictor' with residual of the model without the contribution of predictor}}}"
  [^timeseries.lasso.MultiLasso lasso-model state row]
  (let [current (sp/run lasso-model row)]
    {:statistics (transform-map (:statistics state) [name {v :variance a :average :as arg}] 
                   (let [value (or (get row name) (throw (ex-info "missing sensor value" {:name name})))
                         variance (v value)
                         {:keys [mean sd]} (get-in lasso-model [:statistics name])
                         sd-ratio (/ (Math/sqrt @variance) sd)
                         ;; TODO make threshold configurable
                         variance (if (> sd-ratio 4) ((fade/variance (-> v :acc1 :alpha)) value) variance)
                         avg (a value)]
                     {:variance variance
                      :average avg
                      :deviation (if (zero? sd) 0 (/ (- @avg mean) sd))
                      :sd-ratio sd-ratio}))
     :models (transform-map (:models state) [target' {ps :predictors res :residual vari :variance}]
               (let [prediction (get current target')
                     target-name (feat/find-stem target')
                     target (get row target-name)
                     residual (- target prediction)
                     {:keys [max min]} (get-in lasso-model [:statistics target-name])
                     rel-residual (/ residual (- max min))]
                 {:residual (res rel-residual)
                  :predictors (transform-map ps [predictor cs]
                                (let [value (get row predictor)]
                                  (cs value residual)))}))}))


(defn apply-lasso-models
"Load measurement data from one table, apply LASSO models to each entry. 
Returns {target {predictor-name {:residual fade/average, :correlations fade/correlation}} with correlations of predictors to residuals of target data and the model's prediction."
  [conn columns ingester initial-state plant time-intervals {:keys [max-lag add non-linear?] :or {max-lag 2 add []}}]  
  (let [col-set (set columns)
        ;; if the model does not use the columns that may be summed, we skip those
        f (partial feat/row-wise-added (vec (filter #(every? col-set %) add)))
        f (if non-linear? (comp feat/row-wise-nonlinear f) f)]
    (db/process-raw-values-in-time-range conn plant columns time-intervals
       (fn [rows]
         ;; make sure we use a new lagged-fn per time-interval
        (loop [rows rows,
               state initial-state,
               tis time-intervals,
               lag-fn (feat/row-wise-lagged max-lag),
               drop-count max-lag]
          (let [{ts "timestamp" :as row} (first rows)
                tis' (drop-while (fn [[a b]] (not (time/within? a b ts))) tis)
                new-interval? (not= tis tis')
                lag-fn (if new-interval? (feat/row-wise-lagged max-lag) lag-fn)]
            (cond
              (empty? rows) state
              new-interval? (recur rows state tis' lag-fn max-lag)
              (> drop-count 0) (do ;; remember this row for later lagging and drop it, needed so each row has `max-lag` ancestors
                                 (-> row
                                     (dissoc "timestamp")
                                     f
                                     lag-fn)
                                 (recur (next rows) state tis lag-fn (dec drop-count)))
              ;; add non-linear and lagged features
              :else (recur (next rows) (ingester state (-> row
                                                           (dissoc "timestamp")
                                                           f
                                                           lag-fn
                                                           (assoc "timestamp" ts))) tis' lag-fn 0))))))))


(defn anomaly-candidates-v1
"For debugging purposes."
  [lasso-model
   {:keys [statistics models] :as state}
   {{:keys [residual correlation sd-ratio deviations]
     :or {residual 0.2 correlation 0.8 sd-ratio 0.01 deviations 4}} :thresholds}]
  (let [keep-high-correlations
        (fn [ps]
          (->> ps
               (keep (fn [[predictor corr]]
                       (let [c (Math/abs ^double @corr)
                             ;; if one series is constant, the correlation will be NaN
                             ;; else if we have a high pos/neg correlation, sometimes we
                             ;; get catastrophic cancellation which results in division by zero.
                             ;; In this last case, just assume max. correlation with respective sign
                             c (if (Double/isInfinite c) 0 c)
                             {sdr :sd-ratio dev :deviation} (get statistics predictor)]
                         [predictor {:correlation c :sd-ratio sdr :deviation dev}])))
               (filter (fn [[_ {s :sd-ratio c :correlation d :deviation}]] (or (>= c correlation) (<= s sd-ratio) (>= (Math/abs d) deviations))))
               (into (sorted-map))))]
    (->> models
         (keep (fn [[target {resi :residual ps :predictors}]]
                 (let [avg (Math/abs ^double @resi)
                       stats (get statistics (feat/find-stem target))]
                   (when (>= avg residual)
                         {target {:sd-ratio (:sd-ratio stats)
                                  :deviation (:deviation stats )
                                  :residual @resi
                                  :predictors (keep-high-correlations ps)}}))))
         (apply merge))))

(defn filter-extreme-predictors
"For a map `{name-with-suffix {:sd-ratio num, :correlation num, :deviation num}`
identify the most extreme values per attribute, return new map without name suffixes."
  [preds]
  (->> preds
       (group-by (comp feat/find-stem first))
       (map-values
        (fn [vs]
          (reduce (fn [{c1 :correlation s1 :sd-ratio d1 :deviation} {c2 :correlation s2 :sd-ratio d2 :deviation}]
                    {:correlation (max c1 c2)
                     :sd-ratio (min s1 s2)
                     :deviation (max-key #(Math/abs %) d1 d2)})
                  (map second vs))))))

;; TODO GT19, beginning of january, massive shifts in "Achse1/3 Aktueller Schleppfehler", but heuristic chooses model target "Generatorspannung"
(defn identify-faults
  "Create list of sensors that are anomalous.
  `annotated-state` may be generated via `annotate-sensors`.
Each target ne of three states: constant, shifted, highly correlated.
Cases:
- no shifts, no constant p., no correlated: target
- no shifts, no constant p., one/two correlated: corr. predictors
- target constant, no shifts, no constant p., many correlated: target"
  [annotated-state {{:keys [sd-ratio deviations correlation indications]
                      :or {sd-ratio 0.01 deviations 4 correlation 0.9 indications 3}} :thresholds}]
  (->> annotated-state
   (mapcat (fn [[target {sdr :sd-ratio d :deviation preds :predictors}]]
             (let [preds (filter-extreme-predictors preds)
                   constants  (keep (fn [[p m]] (when (<= (:sd-ratio m) sd-ratio) p)) preds)
                   shifted    (keep (fn [[p m]] (when (>= (Math/abs (:deviation m)) deviations) p)) preds)
                   correlated (keep (fn [[p m]] (when (>= (:correlation m) correlation) p)) preds)
                   |correlated| (count correlated)
                   |shifted| (count shifted)
                   many-corr? (> |correlated| 2)
                   some-corr? (<= 1 |correlated| 2)
                   constants? (not= 0 (count constants))
                   just-me [(feat/find-stem target)]]
               (cond
                 (<= sdr sd-ratio) just-me ;; target constant
                 (>= (Math/abs d) deviations) just-me ;; target shifted
                 (empty? preds) just-me
                 (and many-corr? constants?) constants
                 (not= 0 |shifted|) shifted
                 some-corr? correlated
                 many-corr? just-me))))
   frequencies
   (filter (fn [[k n]] (>= n indications)))
   (into (sorted-map))))


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
  (/ (- sxy (* sx' sy'))v
     (* (Math/sqrt (Math/abs (- sxx (* sx' sx'))))
        (Math/sqrt (Math/abs (- syy (* sy' sy')))))))
 
 )
