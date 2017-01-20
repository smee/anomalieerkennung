(ns scada-ui.components.powercurve
  (:require [reagent.core :as reagent]
            [re-frame.core :refer [register-handler path register-sub dispatch subscribe]]
            [scada-ui.state :as state]
            [scada-ui.components.date-picker :as picker]
            [scada-ui.components.forms :as form]
            [scada-ui.format :as f]
            [scada-ui.i18n :refer [i18n]]
            [cljs-time.core :as time]))


(defn- opts->form-data [opts]
  (let [{:keys [confidence half-life learn-end dates threshold ignore-spikes?]} (merge state/default-power-config opts)
        start (or (:start dates) (time/date-time 2016))
        end (or (:end dates) state/today)
        learn-end (or learn-end (time/date-time 2016 6 1))]
    {:confidence confidence
     :threshold threshold
     :learn-end learn-end
     :dates {:start start :end end}
     :half-life half-life
     :ignore-spikes? (boolean ignore-spikes?)
     :help? false
     ::last-opts opts}))

(defn powercurve-form [opts callback]
  (let [state (reagent/atom (opts->form-data opts))
        confidences [0.999 0.998 0.995 0.99 0.98 0.95 0.9 0.8 0.7 0.6 0.5]]
    (fn [opts callback]
      (when (not (identical? opts (::last-opts @state)))
        (reset! state (opts->form-data opts)))
      (let [help? (:help? @state)]
        [:div 
         [form/help-button state]
         [:div {:class "form"}
          [:fieldset
           [:legend (i18n [:powercurve/learn-duration "Lernzeitraum"])]
           (form/form-row            
            [form/input state
             (i18n [:powercurve/date-range]
                   [(f/human-date (-> @state :dates :start))
                    (f/human-date (-> @state :dates :end))])
             [picker/date-range-picker (reagent/cursor state [:dates])]
             {:show-help? help?
              :help-text (i18n [:powercurve/date-range-help "Die empirische Leistungskennlinie wird für den gesamten Zeitraum ermittelt. Werte davor oder danach stehen für die Visualisierung nicht zur Verfügung"])}]
            [form/input state
             (i18n [:powercurve/learn-end] [(f/human-date (-> @state :learn-end))])
             [picker/day-picker (reagent/cursor state [:learn-end])]
             {:show-help? help?
              :help-text (i18n [:powercurve/learn-end-help "Ab diesem Datum werden neue Leistungswerte nicht mehr für das Lernen berücksichtigt."])}])]           
          [:fieldset
           [:legend (i18n [:powercurve/learn-settings "Lerneinstellungen"])]
           (form/form-row
            [form/input state
             (i18n [:powercurve/threshold "Leistungsschwellwert für Lernen"])
             {:type "number" :min 10 :placeholder "> 0 empfohlen"}
             {:path [:threshold] :show-help? help? :suffix "kW"
              :help-text (i18n [:powercurve/threshold-help "Leistungswerte, die soviel kW unter der mindestens prognostizierten Leistung liegen werden als fehlerhaft angesehen und nicht beim Lernen der empirischen Leistungskennlinie berücksichtigt. Wenn dieser Wert zu klein gewählt ist kann es im Extremfall sein, dass gar keine Werte für das Lernen verwendet werden können."])}]
            [form/input state
             (i18n [:powercurve/half-life "Halbwertszeit der Sensordaten"])
             {:type "number" :min 1 :step 1 :placeholder "> 1"}
             {:path [:half-life]
              :show-help? help?
              :help-text (i18n [:powercurve/half-life-help "Die zu erwartende erzeugte Leistung kann sich im Laufe der Zeit ändern, etwa durch Änderungen der Jahreszeiten, Alterung etc. Es empfiehlt sich daher, nicht die gesamte Historie der Messdaten für die empirische Leistungskennlinie zu verwenden, sondern nur einen begrenzten Teil. Dieser Wert gibt an, wie lang (Einheit: Anzahl Messungen) die Halbwertszeit pro Windgeschwindigkeit ist. Eine Woche bei Messungen aller 10min wäre hier ein Wert von 7 Tage * 24h * 6 Messungen = 1008 zu setzen."])}]
            [form/input state
             (i18n [:powercurve/confidence "Bandbreite in Prozent"])
             (into [:select.form-control {:on-change #(let [idx (.. % -target -selectedIndex)]
                                                        (swap! state assoc :confidence (nth confidences idx)))
                                          :value (:confidence @state)}]
                   (for [c confidences]
                     [:option {:value c} (* c 100)]))
             {:show-help? help?
              :help-text (i18n [:powercurve/confidence-help "Dieser Wert gibt an, wieviel Prozent aller Leistungswerte innerhalb des vorhergesagten Bereiches liegen sollen. Je kleiner dieser Wert, desto häufiger werden die reellen Leistungswerte außerhalb der Vorhersage liegen."])}]
            [form/input state
             (i18n [:powercurve/ignore-spikes? "Einzelne Ausreißer ignorieren"])
             {:type "checkbox"}
             {:path [:ignore-spikes?]
              :show-help? help?
              :help-text (i18n [:powercurve/ignore-spikes-help "Leistungsabweichungen, die nur ein Messinterval lang vorliegen sollten ignoriert werden, so dass längere Zeiträume mit Abweichungen besser erkennbar werden."])}])]           
          [:div.form-group
           [:button {:class "btn btn-default"
                     :type "submit"
                     :on-click #(let [m @state]
                                  ((or callback println) (select-keys m [:confidence :threshold :learn-end :dates :ignore-spikes? :half-life]))
                                  (.preventDefault %))}
            (i18n [:powercurve/submit "Übernehmen"])]]]]))))
