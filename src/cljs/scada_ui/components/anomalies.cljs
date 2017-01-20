(ns scada-ui.components.anomalies
  (:require [scada-ui.pages.common :as pc]
            [reagent.core :as reagent]
            [re-frame.core :refer [register-handler path register-sub dispatch subscribe]]           
            [clojure.string :as str]
            [scada-ui.format :as f]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.components.date-picker :as picker]
            [scada-ui.components.forms :as form]
            [scada-ui.util :refer [map-values]]
            [scada-ui.state :as state]
            [cljs-time.core :as time]))

;; TODO validation either via middleware in handlers or purely via subscriptions?

(def default-config
  {:max-lag 0 ;; maximum lag for lagged series (same series, shifted n values)
   :max-features 15 ;; maximum numbers of predictors per model, more means better but slower learning
   :non-linear? false
   :models-per-target 5 ;; max. number of different models per target
   :topmost 1 ;; number of most important predictors to forbid in next model learning
   :thresholds {:residual 0.2 ;; how big should the average relative residual of target to model output be to be flagged as anomaly?
                :r-squared 0.95 ;; minimum quality for LASSO models, reject lower than this
                :correlation 0.95 ;; minimum correlation of model predictors to residual. observation: if a predictor is the main reason for a large residual, the correlation will be very high
                :deviations 4 ;; max. number of allowed distance of a value from the mean of the learning set measured in standard deviations
                :sd-ratio 0.01 ;; max. ratio of (recent standard deviation)/(training set standard deviation), proxy for detecting constant/stuck sensors
                :ignore-constant 0.2 ;; how long, in percent of the learning time interval, does a series need to be constant to signify, that this sensor indeed has longer terms of constant measurements (needed to distinguish between normal behaviour and 'stuck'/broken sensors)
                :constant-min-length 6 ;; how many consecutive samples does a sensor need to be constant to be included in constant length estimation?
                }             
   :half-life 144 ;; exponential forgetting term for running statistics, see streaming.fading, here: use 1 days halflife
   :half-lifes [1 3 6 12] ;10min, 30min, 60min, 2hours
   :remove {:targets #{}
            :predictors #{}}
   :add #{}
   :dates {:start (time/minus state/today (time/months 1))
           :end state/today}})

(defn- opts->form-data [opts]
  (with-meta (merge default-config opts) {::last-opts opts}))

(defn- subset-opts [state key]
  {:label-fn :label
   :id-fn :series
   :selection-changed-fn (fn [add? ids]
                           (swap! state update-in [:remove key]
                                  (fn [vs] (reduce (if add? conj disj) (set vs) ids))))
   :sorted? true
   :header-all "Alle verfügbaren Sensoren"
   :header-selected "Ausgeschlosse/Ignorierte Sensoren"})

(defn annotation-form [initial-data callback]
  (let [state (reagent/atom (assoc initial-data :help? false))]
    (fn [opts callback]
      (when (not (identical? opts (::last-opts (meta @state) ::not-found)))
        (reset! state (opts->form-data opts)))
      (let [help? (:help? @state)]
        ))))
(defn options-form [initial-data callback]
  (let [state (reagent/atom (assoc initial-data :help? false))
        sensors (subscribe [:all-sensors])]
    (fn [opts callback]
      (when (not (identical? opts (::last-opts (meta @state) ::not-found)))
        (reset! state (opts->form-data opts)))
      (let [help? (:help? @state)
            all-sensors (set @sensors)
            by-series (map-values first (group-by :series all-sensors))
            name->sensor #(by-series (str/replace % "/avg" ""))
            sensors-in-1 (->> @state :remove :targets (map name->sensor) set)
            sensors-in-2 (->> @state :remove :predictors (map name->sensor) set)]
        [:div 
         [form/help-button state]
         [:form {:class "form"}
          [:fieldset
           [:legend (i18n [:lasso/models "Modelle"])]
           (form/form-row
            [form/input state
             (i18n [:lasso/models-count "Anzahl Modelle pro Sensor"])
             {:type "number" :min 1 :max 5 :placeholder "1-5"}
             {:path [:models-per-target]
              :show-help? help?
              :help-text (i18n [:lasso/models-count-help "Wieviele unterschiedliche Modelle sollen im Idealfall pro Sensor gelernt werden? Je mehr, desto größer die Wahrscheinlichkeit, dass Fehler in einem Sensor als Anomalie erkannt werden, aber desto höher auch die Rechenzeit."])}]
            [form/input state
             (i18n [:lasso/topmost "Reduktion pro Nachfolgemodell"])
             {:type "number" :min 1 :placeholder (i18n [:lasso/topmost-placeholder "min. eins"])}
             {:path [:topmost]
              :show-help? help?
              :help-text (i18n [:lasso/tomost-help "Die n besten Prädiktoren eines Modells werden ausgeschlossen, bevor das nächste Modell eines Sensors gelernt wird."])}]
            [form/input state
             (i18n [:lasso/min-quality [:span "Min. Modellqualität R" [:sup "2"]]])
             {:type "number" :min 0.01 :max 1.0 :step 0.01 :placeholder (i18n [:lasso/min-quality-placeholder "Werte < 0.9 erhöhen Fehlalarmrate"])}
             {:path [:thresholds :r-squared]
              :show-help? help?
              :help-text (i18n [:lasso/min-quality-help "Mindestqualität eines gelernten Modells. Wert zwischen 0 und 1, höher bedeutet dass die Werte eines Sensors gut durch das Modell vorhergesagt werden."])}])]
          [:fieldset
           [:legend (i18n [:lasso/features "Features"])]
           (form/form-row
            [form/input state
             (i18n [:lasso/max-lag "Max. Verzögerung"])
             {:type "number" :min 0 :max 6 :step-size 1 :placeholder (i18n [:lasso/max-lag-placeholder "max-lag in 10min"])}
             {:path [:max-lag]
              :show-help? help?
              :suffix "x10min"
              :help-text (i18n [:lasso/max-lag-help "Werte größer 0 bewirken, dass auch Werte aus der Vergangenheit pro Zeitschritt berücksichtigt werden. Dieser Wert bezeichnet die Anzahl von vorangehenden Messwerten, die als Modellprädiktor verwendet werden können. Diese Werte sind z.B. bei zeitlich verschobenen Temperaturanstiegen praktisch."])}]
            [form/input state
             (i18n [:lasso/integral "Summen mit Halbwertszeit"])
             {:type "text" :placeholder (i18n [:lasso/integral-placeholder "Liste von Zahlen, getrennt durch Komma"])}
             {:path [:half-lifes]
              :show-help? help?
              :help-text (i18n [:lasso/integral-help "Addiert die Werte auf, berücksichtigt dabei eine variabel lange Vergangenheit. Jeder Wert steht für eine Halbwertszeit, d.h. eine Halbwertszeit von 3 bedeutet, dass die Summe zur Hälfte die letzten drei Messwerte repräsentiert und zur Hälfte die gesamte vorhergehende Historie."])}]
            [form/input state
             (i18n [:lasso/max-features "Max. Features pro Modell"])
             {:type "number" :min 1 :max 50 :step 1 :placeholder (i18n [:lasso/max-features-placeholder "10-30 ideal"])}
             {:path [:max-features]
              :show-help? help?
              :help-text (i18n [:lasso/max-features-help "Maximale Anzahl der Prädiktoren pro Modell. Wenn dieser Wert zu groß ist werden mehr physikalisch unplausible Zusammenhänge gelernt. Ideal sind Werte im Bereich 10-30."])}]
            [form/input state
             (i18n [:lasso/non-linear "Nichtlineare Features"])
             {:type "checkbox"}
             {:path [:non-linear?]
              :show-help? help?
              :help-text (i18n [:lasso/non-linear-help "Fügt für jeden Sensor abeleitete Werte ein: x^-2, x^-1, x^2, x^3... Erhöht die Rechenzeit dramatisch, erhöht Qualität der Modelle nur geringfügig. Nicht empfohlen!"])}])
           (form/form-row
            [form/input state
             (i18n [:lasso/constants "Zeitweise konstante Werte"])
             {:type "number" :min 0.01 :max 1.0 :step 0.01 :placeholder "0.0 - 1.0"}
             {:path [:thresholds :ignore-constant]
              :show-help? help?
              :suffix "%"
              :help-text (i18n [:lasso/constants-help "Für die Erkennung, ob ein Sensor defekt sein könnte muss unterschieden werden können, ob eine Phase mit konstanten Messwerten ungewöhnlich ist oder nicht. Dazu werden Sensoren mit einem Vermerk versehen, wenn sie min. x% der Referenzzeit konstant waren."])}]
            [form/input state
             (i18n [:lasso/constants-count "Anzahl aufeinander folgender konstanter Werte"])
             {:type "number" :min 2 :step 1 :placeholder "min. 2"}
             {:path [:thresholds :constant-min-length]
              :show-help? help?
              :help-text (i18n [:lasso/constants-count-help "Wieviele direkt aufeinander folgende Werte im Referenzzeitraum müssen konstant sein, damit diese als \"anhaltend konstant\" berücksichtigt werden?"])}])]
          [:fieldset
           [:legend (i18n [:lasso/evaluation "Auswertung"])]
           (form/form-row
            [form/input state
             (i18n [:lasso/half-life "Halbwertszeit der Auswertung"])
             {:type "number" :min 1 :step 1 :placeholder "> 1"}
             {:path [:half-life]
              :show-help? help?
              :help-text (i18n [:lasso/half-life-help "Die Auswertung berücksichtigt nicht die gesamte Datenhistorie, sondern nur einen Teil der Vergangenheit. Die Länge ist dabei nicht fix, sondern Werte werden mit zunehmendem Alter immer geringer gewichtet. Dieser Wert gibt an, wie lang (Einheit: Anzahl Messungen) die Halbwertszeit ist. Eine Woche bei Messungen aller 10min wäre hier ein Wert von 7 Tage * 24h * 6 Messungen = 1008 zu setzen, für einen Tag 1 Tag * 24h * 6 Messungen = 144."])}]
            [form/input state
             (i18n [:lasso/threshold-residual "Schwellwert für Alarmierung"])
             {:type "number" :min 0.01 :step 0.01 :placeholder (i18n [:lasso/threshold-residual-placeholder "in Prozent, typischerweise 0.2"])}
             {:path [:thresholds :residual]
              :show-help? help?
              :help-text (i18n [:lasso/threshold-residual-help "Minimale prozentuale Abweichung von Messwert und Modellvorhersage (bezogen auf den Wertebereich im Lernzeitraum) ab der ein Modell als divergent angesehen wird und für die Ausgabe von Anomalien verwendet werden soll"])}]
            [form/input state
             (i18n [:lasso/threshold-deviations "Verschiebung des Wertebereiches"])
             {:type "number" :min 1 :step 1 :placeholder ">= 4"}
             {:path [:thresholds :deviations]
              :show-help? help?
              :help-text (i18n [:lasso/threshold-deviations-help "Sensordefekte äußern sich häufig durch starke Verschiebungen des Wertebereiches. Dieser Parameter gibt an, ab welcher Verschiebung ein Sensor als potentiell defekt gilt (gemessen in Anzahl Standardabweichungen vom Durchschnitt im Referenzbereich)"])
              :suffix "SD"}]
            [form/input state
             (i18n [:lasso/threshold-sd "Erkennung konstanter Werte"])
             {:type "number" :min 0 :max 1.0 :step 0.01 :placeholder (i18n [:lasso/threshold-sd-placeholder "möglichst klein, nahe 0"])}
             {:path [:thresholds :sd-ratio]
              :show-help? help?
              :help-text (i18n [:lasso/threshold-sd-help "Minimales Verhältnis von aktueller Standardabweichung zu Standardabweichung im Referenzbereich unterhalb dessen der Sensorwert als konstant angesehen werden kann."])}])]
          [:fieldset
           [:legend (i18n [:lasso/sensors "Sensoren"])]
           ;;remove targets

           [form/input state
            (i18n [:lasso/exclude-targets "Kein Modell für Sensoren lernen"])
            [form/subset-of-list all-sensors sensors-in-1 (subset-opts state :targets)]
            {:show-help? help?
             :help-text (i18n [:lasso/exclude-targets-help "Manche Sensoren können prinzipiell nicht gut mit Hilfe der Werte von anderen Sensoren vorhergesagt werden. Diese Sensoren sollten explizit ausgeschlossen werden, um die Fehlalarmrate zu senken. Beispiel: Sensoren die Vorgaben statt Messwerte darstellen"])}]
           ;;remove predictors

           [form/input state
            (i18n [:lasso/exclude-sensors "Sensoren als Prädiktoren ausschließen"])
            [form/subset-of-list all-sensors sensors-in-2 (subset-opts state :predictors)]
            {:show-help? help?
             :help-text (i18n [:lasso/exclude-sensors-help "Manche Sensoren erhöhen die Fehlalarmwahrscheinlichkeit und sollten explizit nicht verwendet werden. Beispiele: Zähler, die im Lernzeitraum kontinuierlich ansteigen, später dann aber per Reset wieder auf 0 gesetzt werden."])}]
           [form/input state
            (i18n [:lasso/added-sensors "Redundant betiebene Sensoren aufaddieren"])
            [form/subset-of-list all-sensors (reduce (fn [res ids] (conj res (mapv name->sensor ids))) #{} (:add @state))
             {:label-fn (fn [m] (cond
                                 (map? m) (:label m)
                                 (vector? m) (str/join "/" (map :label m))
                                 :else "???"))
              :id-fn (fn [m] (cond
                              (map? m) (:series m)
                              (vector? m) (str/join "+" (map :series m))
                              :else "???"))
              :selection-changed-fn (fn [add? ids]
                                      (swap! state update-in [:add]
                                             #(if add?
                                                (conj % (set ids))
                                                (reduce (fn [st ids]
                                                          (disj st (set (str/split ids "+")))) % ids))))
              :sorted? true
              :header-all (i18n [:lasso/all-available-sensors "Alle verfügbaren Sensoren"])
              :header-selected (i18n [:lasso/all-added-sensors "Aufaddierte Sensoren"])}]
            {:show-help? help?
             :help-text (i18n [:lasso/added-sensors-help "Manche Komponenten sind entweder redundant oder arbeiten abwechselnd. Statt ihrer jeweiligen Einzelwerte sollte die Summe der jeweiligen Sensoren verwendet werden. Beispiel: Wenn zwei Kühlwasserpumpen abwechselnd arbeiten ist der Gesamtdruck des Kühlwassers proportional zur Summe der Ströme beider Pumpen."])}]]

          [:fieldset
           [:legend (i18n [:lasso/learn-duration "Lernzeitraum"])]
           [form/input state
            (i18n [:lasso/duration-label "Referenzzeitraum/Normalbetrieb (%1 - %2"]
                  [(-> @state :dates :start f/human-date) (-> @state :dates :end f/human-date)])
            [picker/date-range-picker (reagent/cursor state [:dates])]
            {:show-help? help?
             :help-text (i18n [:lasso/learn-duration-help "Alle Messwerte aus den Produktivzeiten in diesem Referenzzeitraum werden als Normalität für die Anomalieerkennung betrachtet. Der Ausschnitt sollte also repräsentativ für den Betrieb sein, speziell hinsichtlich der Windbedingungen. Wenn nur mit Starkwind gelernt wird kann es später viele Fehlalarme in Zeiten mit schwachem Wind geben und umgekehrt."])}]]
           ;;add up sensors, virtual sensors

          [:div.form-group
           [:button {:class "btn btn-default"
                     :type "submit"
                     :on-click #(do (.preventDefault %) ((or callback println) @state))}
            "Start"]]]]))))

