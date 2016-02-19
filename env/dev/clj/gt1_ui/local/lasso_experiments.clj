(ns gt1-ui.local.lasso-experiments
  (:require [chart-utils
             [colors :as cuc]
             [jfreechart :as cjf]]
            [clj-time
             [coerce :as tc]
             [core :as time]]
            [clojure
             [inspector :refer [inspect-tree]]
             [set :as set]
             [string :as str]]
            [gt1-ui.defaults :as defaults]
            [gt1-ui handler 
             [bayes :as bayes]
             [lasso :as gl]]
            [gt1-ui.data
             [availability :as avail]
             [db :as db]]
            [gt1-ui.local.charts :as charts]
            [org.clojars.smee.map :refer [map-values]]
            [streaming
             [fading :as fade]
             [protocols :as sp]]
            [timeseries
             [features :as feat]
             [functions :refer [normalizing-constants]]
             [lasso :as lasso]]
            [timeseries.lasso.lasso-plots :as lp]))

;; will be defined interactively, FIXME all are global variables....
(declare timestamps models columns obs-as-maps name->label names opts)

(defn deref-all [x]
  (clojure.walk/prewalk #(if (instance? clojure.lang.IDeref %) (deref %) %) x))
(defn sort-each-map [m]
  (clojure.walk/postwalk #(if (map? %) (into (sorted-map) %) %) m))
(defn format-numbers [m]
  (clojure.walk/postwalk #(if (and (number? %) (Double/isFinite %))
                            (-> % bigdec (.setScale 3 BigDecimal/ROUND_HALF_UP))
                            #_(-> % double (* 1000.0) (Math/round) (/ 1000.0))
                            #_(format "%.3f" (double %))
                            %) m))
(defn add-labels [m]
  (clojure.walk/postwalk #(if (string? %)
                            (if-let [lbl (name->label (feat/find-stem %))]
                              (str lbl "-" %)
                              %)
                            %) m))

(def nicer (comp format-numbers sort-each-map deref-all))

(defn load-series-metadata [conn plant]
  (->> conn
       db/get-all-series-metadata
       (filter #(= plant (:plant %)))
       (filter #(#{"avg"} (:statistics %)))
       (sort-by :series)))

(defn select-most-important-feature [n lasso-model]
  (->> lasso-model 
    :parameters 
    :attributes 
    (sort-by (comp :importance second))
    (take-last n)
    (map first)))

(defn partial-correlation
"Partial correlation of X and Y given Z."
  [rxy ryz rxz]
  (/ (- rxy (* ryz rxz))
     (Math/sqrt (* (- 1 (* ryz ryz)) (- 1 (* rxz rxz))))))



(defn plot-model
  "plot individual models"
  ([target-name] (plot-model target-name {:models  models :correlations? true}))
  ([target-name' {:keys [models start end correlations?] :or {start 0 end (count obs-as-maps)}}]
   (when-let [model (get (:models models) target-name')]
     (let [target-name (feat/find-stem target-name')
           threshold (-> opts :thresholds :correlation)
           alpha (:alpha opts)
           observations (take (- end start) (drop (dec start) obs-as-maps))
           ts (map :timestamps observations)
           target (mapv #(get % target-name) observations)
           est (mapv model observations)
           title (str (name->label target-name) " - " target-name')
           residual (mapv - target est)           
           {mi :min ma :max} (get-in models [:statistics target-name])
           residual-percentages (mapv #(/ % (- ma mi)) residual)
           orig-sd (-> models :statistics (get target-name) :sd)
           variances (reductions #(let [v (%1 %2)
                                        sd (Math/sqrt @v)]
                                    ;; if there is a massive spike, maybe we should forget the variance
                                    ;; and start fresh, else it would take a long time until the variance
                                    ;; comes back down
                                    (if (> (/ sd orig-sd) 4) ((fade/variance alpha) %2) v))
                                 (fade/variance alpha) target)           
           variances (mapv #(/ (Math/sqrt (deref %)) orig-sd) variances)
           avgs (first (reduce (fn [[res avg] d] (let [avg' (avg d)] [(conj res @avg') avg'])) [[] (fade/average (-> opts :alpha))] residual-percentages))
           attributes (-> model :parameters :attributes)
           predictor-names (->> attributes keys set)
           indicators (into {}
                            (for [[p {w :weight i :importance orig-sd :sd}] attributes
                                  :let [corr1 (reduce (fn [c [row residual]]
                                                        (let [c' (c (get row p) residual)]
                                                          ;(when (= p "2/10/5/avg") (println @c'))
                                                          c'))
                                                         (fade/correlation alpha) (map vector observations residual))
                                           c1 @corr1
                                           c1 (cond (Double/isNaN c1) (do (println "warning: correlation with residual of p is NaN!" corr1) 0)
                                                    (Double/isFinite c1) c1
                                                    :else (do (println "warning: correlation with residual of p is infinite!" p corr1) (Math/signum c1)))]]
                                 [p {:correlation-with c1
                                     :series p
                                     :label (name->label (feat/find-stem p))}]))
           indicators (reduce-kv (fn [m p {c1 :correlation-with :as crnt}]
                                   (let [p-stem (feat/find-stem p)
                                         similar-preds (filter #(= p-stem (feat/find-stem %)) predictor-names)
                                         other-preds (apply disj predictor-names similar-preds)
                                         c2s (reduce (fn [corrs [row res]]
                                                       (reduce-kv (fn [m other ct]
                                                                    (assoc m other (ct (get row other) (get row p))))
                                                                  {} corrs))
                                                     (zipmap other-preds (repeat (fade/correlation alpha)))
                                                     (map vector observations residual))]
                                     (assoc m p (assoc crnt :partial-correlations
                                                       (reduce-kv (fn [m other ct]
                                                                    (let [ct @ct
                                                                          cr (get-in indicators [other :correlation-with])
                                                                          pc (partial-correlation cr c1 ct)]
                                                                      (if (>= (Math/abs pc) 0.6)
                                                                        (assoc m other pc)
                                                                        m)))
                                                                  {} c2s)))))
                                 {} indicators)]
       ;; plot model, residual
       (->
        (charts/plot-parsed-data ts {target-name target :estimation est} {:title title})
        (incanter.charts/add-lines ts residual-percentages)
        (cjf/map-to-axis 2 1)
        (incanter.charts/add-lines ts avgs :series-label "fading averages")
        (cjf/map-to-axis 3 1)
        (incanter.charts/add-lines ts variances :series-label "sd ratios")
        (cjf/map-to-axis 4 1)
        #_(cjf/add-value-marker (-> opts :thresholds :residual) "alarm threshold");doesn't work with 2nd y-axis
        cjf/set-discontinuous)
       ;; correlations histogram
       ;(incanter.core/view (incanter.charts/histogram (filter #(Double/isFinite %) (map :correlation-with (vals indicators))) :nbins 20 :title "Correlation residual/predictors with"))
       (when correlations?
         (->> indicators
             vals
             (sort-by :correlation-with)
             reverse
             format-numbers
             clojure.inspector/inspect-table))))))


(defn describe-model
  "Tabular description of a model."
  [model-name]
  (when-let [model (get (:models models) model-name)]
    (let [params (->> model :parameters :attributes)
          p-names (keys params)
          p-labels (mapv (comp name->label #(feat/find-stem %)) p-names)
          p-weights (map :importance (vals params))]
      (printf "%s - %s (r²=%.4f)\n" model-name (name->label (feat/find-stem model-name)) (-> model :parameters :r-squared))
      (clojure.pprint/print-table [:name :label :importance :has-model?]
                                  (->> p-weights
                                       (map #(hash-map :name %1 :label %2 :importance (format "%.4f" %3) :has-model? (boolean (some (fn [^String s] (.startsWith s (feat/find-stem %1))) (keys (:models models))))) p-names p-labels)
                                       (sort-by :importance)
                                       reverse)))))




(defn plot-predictors
  "Plot all predictor time series."
  [model-name]
  (cjf/set-discontinuous
   (charts/plot-parsed-data
    (mapv tc/to-long timestamps)
    (->> model-name
         (get (:models models))
         sp/input-names
         (select-keys columns)
         (map-values #(format "%s (%s)" (name->label %) %) identity)
         ;; FIX for bug in core.matrix: does not handle instances of `vector-of` correctly
         (map-values (partial into [])))
    {:title (name->label model-name)})))


(defn load-metadata! [conn plant]
  (let [md (load-series-metadata conn plant)
        names (mapv :column-name md)]
    (def name->label (zipmap names (map :label md)))
    (def names names)))

(defn load-data! [conn plant start end]
    ;; load data
  (let [time-intervals (avail/operation-time-intervals conn plant start end)
        {:strs [timestamp] :as columns} (db/raw-value-in-time-range conn plant names time-intervals)
        columns (dissoc columns "timestamp")]
    (def segment-lengths (mapv count (partition-by #(some (fn [[from to]] (when (time/within? from to %) from)) time-intervals) timestamp)))
    (def timestamps timestamp)
    (def columns columns)))



(defn columns->rows [columns timestamps segment-lengths opts]
  (let [columns (cond-> columns
                  (not-empty (:add opts)) (feat/add-added-columns (:add opts))
                  (:non-linear? opts) (merge (feat/derive-nonlinear-features columns #_Double/MAX_VALUE))
                  true (feat/add-lagged-columns segment-lengths (:max-lag opts))
                  true (assoc :timestamps (map tc/to-long timestamps)))
        ks (keys columns)
        vs (vals columns)]
    (for [row (apply map vector vs)]
      (apply hash-map (interleave ks row)))))

(defn show-models! [models]
  (let [colors (cuc/divergence-colorscale -1 1)
        n (count obs-as-maps)
        plot-data (lp/render-models obs-as-maps
                                    (update-in models [:models] (partial into (sorted-map)))
                                    colors
                                    n
                                    [])
        handler (fn handler [x target-name]                           
                  (let [model (get-in models [:models target-name])]
                    (plot-model target-name (- x 500) (+ x 500))
                    (describe-model target-name)))]
    (lp/pannable plot-data handler)
    plot-data))


(defn models-histogram [models]
  (let [vs (->> models :models vals (map (comp count :attributes :parameters)))]
    (-> vs        
        (incanter.charts/histogram :nbins 30 :title (str (count vs) " models"))
        incanter.core/view)))

(defn models-with-predictor
"For debugging: Focus only on a subset of models/statistics: Needs to be related to `predictor`."
  [models predictor]
  (let [all-names (sp/input-names models)
        models' (into {} (for [[target model] (:models models)
                              :when (or (= (feat/find-stem predictor) target)
                                        (some #(= predictor (feat/find-stem %)) (-> model :parameters :attributes keys)))]
                          [target model]))]
    {:models models' :statistics (select-keys (:statistics models) (vec all-names))}))

(defn models-like [^String s]
  (filter (fn [^String k] (.contains k s)) (keys (:models models))))

(defn reverse-model
"A regression model can be interpreted as a relation between one target variable and many predictor variables.
This function derives a regression model for one of the predictor variables by solving the regression
formula for `predictor`."
  [target {{:keys [intercept attributes]} :parameters} predictor]
  (let [weight_p (get-in attributes [predictor :weight])
        ps (for [[name {w :weight}] attributes :when (not= name predictor)] [name {:weight (/ w weight_p -1)}])
        target-p [(feat/find-stem target) {:weight (/ 1.0 weight_p)}]]
    (timeseries.lasso/map->LassoModel {:parameters {:intercept (/ intercept weight_p -1)
                                                    :attributes (into {} (cons target-p ps))}})))
(defn reverse-models
"Identify all models that have exactly one predictor with name `predictor`. Create
reverse LASSO regression models for these."
  [models predictor]
  (let [mwp (for [[target model] (:models (models-with-predictor models predictor))
                  :let [candidates (->> model :parameters :attributes keys (filter #(= predictor (feat/find-stem %))) set)]
                  ;; ignore models that have either derived predictors or multiple instances of the predictor
                  ;; we can't reverse models for these cases
                  ;; also, ignore models where the relative fraction of the predictor is too small
                  ;; reason: the reverse model will not actually predict only the predictor value but rather the sum of the predictor and the residual of the original model (y-y_hat).
                  :when (and (= candidates #{predictor})
                             (>= (get-in model [:parameters :attributes predictor :importance]) 0.1))]
              [(str predictor "|derived_" target)
               (reverse-model target model predictor)])]
    (assoc models :models (into {} mwp))))

(comment

  (let [unwanted-preds #{"2/13/11/avg" ;;Anzahl Hübe Azimutlager Schmierung, zwar nicht konstant im Trainingszeitraum, aber typischerweise über längere Zeiträume
                         "2/13/12/avg" ;;Anzahl Hübe Azimutritzel Schmierung
                         "3/20/27/avg" ;;Anzahl Hübe Blattlager Schmierung
                         "3/20/28/avg" ;;Anzahl Hübe Blatt Ritzel Schmierung
                         "2/12/10/avg" ;; Hydraulikdruck Rotorbremse Kreis B, gebraucht zum Bremsen, in Produktion zufälliges Rauschen
                         "2/12/9/avg" ;; Hydraulikdruck Rotorbremse Kreis A
                         "2/15/15/avg" ;; Turmbiegeeigenfrequenz, periodisch ermittelt, festgesetzt
                         "2/12/13/avg" ;; Arretierzylinder 1 Position
                         "2/12/14/avg" ;; Arretierzylinder 2 Position
                         "2/12/15/avg" ;; Arretierzylinder 3 Position
                         "1/2/17/avg" ;; Leistungsmessung 10Min Intervall aktiv
                         "2/10/59/avg" ;;undokumentiert, seit November vorhanden
                         "2/10/60/avg" ;;undokumentiert, seit November vorhanden
                         "2/10/61/avg" ;;undokumentiert, seit November vorhanden
                         }]
    (def opts {:max-lag 2 ;; maximum lag for lagged series (same series, shifted n values)
               :max-features 20 ;; maximum numbers of predictors per model, more means better but slower learning
               :max-edit-distance 0 ;; remove all features if their name differs only by up to n characters
               :non-linear? false
               :models-per-target 5 ;; max. number of different models per target
               :topmost 2 ;; number of most important predictors to forbid in next model learning
               :keep-fit? false ;; keep the LASSO fit object for later analysis, not needed in production
               :thresholds {:residual 0.2 ;; how big should the average relative residual of target to model output be to be flagged as anomaly?
                            :r-squared 0.9 ;; minimum quality for LASSO models, reject lower than this
                            :correlation 0.9 ;; minimum correlation of model predictors to residual. observation: if a predictor is the main reason for a large residual, the correlation will be very high
                            :probability 0.7 ;; all sensors with a failure probability below this value will be ignored
                            :importance 0.01 ;; minimum normalized weight of a regression model feature
                            :deviations 4 ;; max. number of allowed distance of a value from the mean of the learning set measured in standard deviations
                            :sd-ratio 0.01 ;; max. ratio of (recent standard deviation)/(training set standard deviation), proxy for detecting constant/stuck sensors
                            :indications 3 ;; min. number of times a sensor needs to be identified as root cause for an individual lasso model deviation, reduces number of false positives
                            }             
               :alpha (streaming.fading/half-life->alpha 72) ;; exponential forgetting term for running statistics, see streaming.fading, here: use 0.5 days halflife
               :remove {:targets (into unwanted-preds
                                       #{"2/13/9/avg" ;;Kabelverdrillung, zustandsbehaftet
                                         "1/1/37/avg" ;; PLS Vorgabe Drehzahl
                                         "1/1/35/avg" ;; PLS Vorgabe Wirkleistung
                                         "1/1/36/avg" ;; PLS Vorgabe Blindleistung
                                         })
                        :predictors unwanted-preds}
               :add [["2/11/28/avg" "2/11/29/avg"] ;; Motorstrom Kühlwasserpumpe 1/2
                     ["2/10/13/avg" "2/10/14/avg" "2/10/15/avg"] ;; Motorstrom Getriebeölpumpe A/B/C
                     ["2/10/51/avg" "2/10/52/avg"] ;; Motorstrom Ölpumpe Durchlauferhitzer A/B
                     ["2/11/20/avg" "2/11/21/avg" "2/11/22/avg" "2/11/23/avg"] ;; Motorstrom Generatorlüfter 1/2/3/4
                     ]}))
  (def plant "GT32") ;; TODO GT04 should show anomaly at 3/22/9, 3/20/12 higher than 3/20/13 and 3/20/14, probability too low, though (01.02-21.02)
  ;; TODO visualize model options, learning time interval in anomalies page. GT04 for example had many manual stops during aug-dec 2015
  (def conn (or (db/connection "remote")
                (db/create-connection-pool! "remote" (db/default-spec "remote" "mysql://root:c5bf19d0918e02c1@localhost:28850/gt1-db"))))

  (def conn gt1-ui.handler/conn)
  
  (do
    (def obs-as-maps nil)
    (load-metadata! conn plant)
    (load-data! conn
               plant
               ;(time/date-time 2015 6 10)
               ;(time/date-time 2015 9 19)
               (time/date-time 2016 2 1)
               (time/date-time 2016 3 1)
               ;(time/date-time 2016 1 14)
               ;(time/date-time 2016 1 25)
               ;(time/date-time 2016 2 8)
               )
    (def obs-as-maps (columns->rows columns timestamps segment-lengths opts)))

  ;; learn new models
  (def models (gl/learn-lasso-models (dissoc columns "timestamps") segment-lengths
                                     (assoc opts
                                            :debug? true
                                            :name->label name->label)))

  ;; learn a model for each plant
  (let [from (time/date-time 2015 8 1)
        to (time/date-time 2015 12 31)]
    (load-metadata! conn "GT01")
    (doseq [plant #_[plant] (map #(format "GT%02d" %) (range 1 81))
            :when (empty? (db/algorithm conn plant))
            :let [_ (do (printf "%s\n%s\n" (apply str (repeat 80 \=)) plant) (flush))
                  _ (load-data! conn plant from to)
                  len (apply + (map #(Math/abs (- % (:max-lag opts))) segment-lengths))
                  models (time (gl/learn-lasso-models (dissoc columns "timestamps")
                                                      segment-lengths
                                                      (assoc opts :name->label name->label)))]]
      (def models models)
      (def plant plant)
      (db/store-algorithm! conn plant from to models opts))
    (time (gt1-ui.data.etl/run-algorithms! conn)))
  
  (show-models! models) 
  (models-histogram models)
  
  ;; serialization of models
  
  (def models (ser/deserialize "/home/steffen/daten/gt1/models/gt06-added"))
  (def models (:algorithm (db/algorithm conn plant)))
  (def opts (:options (db/algorithm conn plant)))

  (require '[org.clojars.smee.serialization :as ser])
  (ser/serialize "/home/steffen/daten/gt1/models/gt06-added" models)
  (db/store-algorithm! conn plant (time/date-time 2015 6 10) (time/date-time 2015 9 19) models opts)

  (db/do! conn
          ;"truncate table `algorithms`"
          ;"truncate table `algorithm-states`"
          ;"truncate table `algorithm-results`"
          ;(format "delete from `algorithms` where plant='%s'" plant)
          ;(format "delete from `algorithm-states` where plant='%s'" plant)
          ;(format "delete from `algorithm-results` where plant='%s'" plant)
          )
  (def opts (update-in opts [:remove :predictors] conj "1/3/13/avg")) ;; Temperatur Zuluft offensichtlich defekt
  ;; automatic evaluation, step 1: calculate average relative residual per model, correlation of individual predictors to relative residual per model
  
  (def state (let [;models (update-in models [:models] select-keys ["2/14/1/avg"])
                   ingester (partial gl/ingest-new-measurement models)
                   time-intervals (avail/operation-time-intervals conn plant
                                                                  (time/date-time 2016 2 1)
                                                                  (time/date-time 2016 2 25))]
               (time
                (gl/apply-lasso-models conn
                                       (set (mapcat feat/find-stems (sp/input-names models)))
                                       ingester
                                       (gl/initial-state models opts)
                                       plant
                                       time-intervals
                                       opts))))
  ;; step 2: extract anomalous serieses (plot average relative residual)
  (-> (map (comp deref :residual)(vals (:models state)))
      (incanter.charts/histogram :nbins 300)
      (incanter.charts/set-axis :y (incanter.charts/log-axis))
      incanter.core/view)

(inspect-tree (nicer state))
(inspect-tree (nicer models))  
  ;; step 3: apply heuristic to determine the probable anomalous series, debug
(-> (gl/anomaly-candidates-v1 models state opts)
    ;(gl/identify-faults opts)
     add-labels
     nicer
     inspect-tree) 
  

  ;; step 4: apply heuristic to determine the probable anomalous series
  
 (-> (gl/anomaly-candidates-v1 models state opts)
    (gl/identify-faults opts)
     add-labels
     nicer
     inspect-tree)
 
 
   
 ;; matrix for visualization
  (def adj-m (zipmap (keys models)
                     (->> models vals (map (comp vec #(for [[k {w :importance}] %] {:name k :weight w}) :attributes :parameters)))))

  ;; plot reverse models
  (let [ms (reverse-models models "1/3/12/avg")] (doseq [n (keys (:models ms))] (plot-model n {:models ms})))
  (run! #(plot-model % {:models models :correlations? true}) (models-like "1/3/12"))
  
  )



;; TODO GT49, "Luftfeuchte Luftaufbereitung Zuluft" 1/3/14 ist verdaechtig niedrig seit Mitte August
;; trainiert mit Daten vom Juni, Anzeige Juni-August

;; GT29 gelernt 10.06-10.09.2015, massive Ausschläge ab 17.09. Grund: Zusammenhang mit "PLS Vorgabe Blindleistung" (vorher konstant 0, ab da größer 0)
;; => Fazit: Zeitreihen, die (nahezu) konstant sind sollten vielleicht ausgeschlossen werden als Prädiktoren?

;; TODO GT29, gelernt 10.06-19.09, Anstieg "Wasserdruck PT1/2" ab 27.09., keine Parameteränderung sichtbar in ParaChange
;; TODO GT29, gelernt 10.06-19.09, Anstieg "Achse1 Spannung Batterie 100%" et.al. ab 27.09., keine Parameteränderung sichtbar in ParaChange


;; GT61 Mitte August-Mitte September gelernt, ab 19.09. Erhöhung "Hydraulikdruck Rotorbremsspeicher" und "Hydraulikdruck Rotorbremse",
;; Beobachtung mögliche Einstellungsänderung (ja, siehe OpChange: von [850,880] zu [720,750] für Steuerung Hydraulikpumpe, Minimaler/Maximaler Druck Speicher Rotorbremse Ein/Aus)

;; TODO GT61 Wasserdruck PT2 ab Mitte September zu niedrig, 19.10. und 05.11. starke Anstiege

;; TODO GT11 1/3/13/avg Seit Mitte September abfallend, nicht erklärbar durch Prädiktoren, ab Dezember inplausibel, Sensordefekt? falls ja, dann Ankündigung seit Mitte September!
;; Beobachtung: Bei Abweichungen von Modell und Messwert ohne Erklärungen unter den Prädiktoren könnte eine Trendüberwachung interessant sein
;; TODO GT11 Achse3 Aktueller Schleppfehler steigt ab Dezember an
;; TODO GT11 "Hydrauliköl Temperatur 1/2" ist zeitweise deutlich höher als in anderen Anlagen?
;; TODO GT32 "Temperatur Luftaufbereitung Zuluft" ab 08.01. angestiegen, Abfall "Druckdifferenz Lüftung 2. Filterstufe A 2"

;; Beobachtung: zur Bestimmung des wahrscheinlichsten, die Änderung in den Residuen verursachenden Prädiktors eignet sich die Korrelation aus Prädiktor und Residuen besser als die Korr. aus Prä. vs. Res-w*Prä. Zweiteres wird zu schnell dominiert von normalen, gewünschten Zusammenhängen


;; TODO 20160413 GT32, 1/3/12/avg|v1/3 et. al haben Modelabweichungen Anfang und Ende Februar. Augenscheinlich auf 1/3/4/avg zurueckzufuehren, aber wird nicht durch die aktuellen Heuristiken erkannt (keine Verschiebung, nicht konstant, nicht stark korreliert...)


;; Beobachtung: es gibt auch Traces ohne Fehlereintrag in opmodes, z.B. GT01, 01.12.2015 09:08:55, 10.11.2015 beide

(comment
  (let [cs (feat/add-lagged-columns (merge columns (feat/derive-nonlinear-features columns)) 2)]
    (with-open [^java.io.ObjectOutputStream os (-> "d:/lasso" clojure.java.io/output-stream java.io.ObjectOutputStream.)]
      (.writeInt os (count cs))
      (.writeInt os (count (first (vals cs))))
      (run! #(.writeObject os (float-array %)) (vals cs))))

)

(comment

  ;; plot to test time until fading variance
  (let [wave (range 0 (* 5 Math/PI) 0.01)
        n 600
        xs (range (+ n (count wave)))
        prefix (map #(Math/sin %) wave)
        orig-sd (timeseries.functions/sd wave)
        chart (doto (incanter.charts/xy-plot xs [0] :title "Recognize constant values demo" :legend true :series-label "<- data")
                (incanter.charts/add-lines xs [0] :series-label "<- standard deviations" )
                (incanter.charts/add-lines xs [0] :series-label "-> ratio of recent SD/learned SD")
                (cjf/map-to-axis 2 1)
                incanter.core/view)]
    (cjf/sliders
     [alpha [1.0 0.9999 0.999 0.99 0.98 0.95 0.9]
      stuck-value [0 -40 -5 -2 -1 1 2 5 100]]
     (let [vs (concat prefix (repeat n stuck-value))
           var (streaming.fading/variance alpha)
           vars (map #(Math/sqrt (deref %)) (reductions #(%1 %2) var vs))]
       (doto chart
         (cjf/perf-set-data [xs vs] 0)
         (cjf/perf-set-data [xs vars] 1)
         (cjf/perf-set-data [xs (map / vars (repeat orig-sd))] 2)))))
  )
