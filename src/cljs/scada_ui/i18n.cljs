(ns scada-ui.i18n
  (:require [taoensso.tempura :as tempura]
            [re-frame.core :refer [subscribe reg-sub reg-event-db]]))

(def dict
  {:en {:missing "!!! MISSING !!!"
        :please-wait "Please wait..."
        :column "Column"
        :from "from"
        :with "with"
        :without "without"
        :all "all"
        :none "none"
        :start "Start"
        :end "End" 
        :wind-rose-tooltip "%1% from %2°, speed \u2205 %3m/s"
        :loading "Loading..."
        :date "Date"
        :plant "WEC"
        :plants "WECs"
        :since "Since"
        :sensor "Sensor"
        :sensors "Sensors"
        :label "Label"
        :description "Description"
        :unit "Unit"
        :physical-dimension "Physical dimension"
        :new "New"
        :old "Old"
        :component "Component"
        :error "Error"
        :errors "Errors"
        :code "Code"
        :user "User"
        :kind "Kind"
        :mode "Modus"
        :on "on"
        :off "off"
        :calendar "Calendar"
        :language "Language"
        :selection "Selection: \"%1\""
        :forms {:all-entries "All available entries"
                :selected "Selected"
                :deselect "Deselect"
                :select "Select"
                :toggle-help "Show/hide help texts"}
        :time {:time "Time"
               :period "Period"
               :duration "Duration"
               :day "day"
               :days "days"
               :hour "hour"
               :hours "hours"
               :minute "minute"
               :minutes "minutes"
               :second "second"
               :seconds "seconds"
               :weekday-short #(nth ["Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"] (first %1))
               :weekday-long #(nth ["Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"] (first %))
               :months (fn [] ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"])
               :months-long #(nth ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"] (first %1))
               :last-week "Last week"
               :last-month "Last month"
               :last-n-months "Last %1 months"
               :last-year "Last year"}
        :titles {:availability "Availability"
                 :scadadata "Measurements"
                 :anomalies "Anomalies"
                 :sensors "Sensors"
                 :wind "Wind Statistics"
                 :powercurve "Power Curve"}
        :modes-page {:choose "Please choose a power plant:"
                     :mode "Operation mode"
                     :durations "Duration in operation mode"
                     :per-day "\"%1\" per day"
                     :color-scale "Color scale"
                     :legend "Legend"
                     :title "Overview per day"
                     :errors "Error messages"
                     :no-errors "no errors"}
        :wind-page {:title "Distribution of Wind Direction and Speeds"
                    :angle "Sector angle:"
                    :apply "Apply"
                    :instructions "Please select an instance to see detailed statistics."}
        :anomalies {:button-tooltip "Show as interactive chart"
                    :deviation "Value shift"
                    :deviation-tooltip "Shift of values of more than x standard deviations in relation to learning data"
                    :deviation-title "Value shift"
                    :sdr-tooltip "Ratio of the standard deviations of this value of the recent past and the learning data"
                    :target "Model target"
                    :target-tooltip "Regression model for this sensor"
                    :predictors "Predictors"
                    :predictor-tooltip "Name of the predictor of a regression model"
                    :sensor-name "Sensor name"
                    :correlation "Correlation"
                    :correlation-tooltip "Correlation of the sensor value with the residual of model prediction minus sensor value"
                    :select-wec "Select a WEC"
                    :reference-period "Reference period"
                    :no-config "no config"
                    :min-hints "Minimal hints"
                    :all-results "All results"
                    :all-results-tooltip "Show all anomaly candidates on days, where the heuristic could not determine a likely reason. This switch has the same effect as setting \"minimal hints\" to 1."
                    :time-interval "Time interval"
                    :switch-all "Switch all reference models"
                    :settings "Settings"
                    :anomalies "Anomalies"
                    :number-hints "Number of hints"
                    :models "Models"
                    :models-tooltip "Details of divergent regression models"
                    :details-title "Details of anomaly detection"
                    :evaluation "Evaluation"
                    :threshold-residual "Residual threshold"
                    :threshold-deviations "Shift of value range"
                    :threshold-sd "Threshold for stuck values"
                    :occurances [:span "**%1** (%2/%3 models, %4 predictors"]
                    :change-type "Message"
                    :too-high "too high"
                    :too-low "too low"
                    :duration-in-day "Duration in day"}
        :lasso {:models "Models"
                :models-count "Number of models per sensor"
                :topmost "Topmost predictors to remove"
                :min-quality [:span "Min. quality R" [:sup "2"]]
                :features "Features"
                :max-lag "Maximum lag"
                :integral "Sums with half-life"
                :max-features "Max. features per model"
                :non-linear "Nonlinear features"
                :constants "Partially constant values"
                :constants-count "Number of successive constant values"
                :sensors "Sensors"
                :exclude-targets "Do not learn models for "
                :exclude-sensors "Exclude sensors as predictors"
                :added-sensors "Add up redundant sensors"
                :all-available-sensors "All sensors available"
                :all-added-sensors "Added sensors"
                :learn-duration "Learning date range"
                :duration-label "Reference date range / normal operation (%1 - %2"
                :relative-weight "Relative weight"}
        :tree {:search-term "search term"
               :filter "Filter"
               :expand "Expand selected element"
               :expand-selected "Expand subtrees that contain selected elements"
               :collapse "Collapse selected element"
               :drain "Clear selected elements"
               :Komponenten "Components"
               :Physikalisch "Physical dimensions"
               :Anteil "Proportion"
               :Ausrichtung "Attitude"
               :Beschleunigung "Acceleration"
               :Blindleistung "Reactive power"
               :Druck "Pressure"
               :Energie "Energy"
               :Frequenz "Frequency"
               :Geschwindigkeit "Speed"
               :Kraft "Torque"
               :Leistung "Power"
               :Rotationsgeschwindigkeit "Rotational speed"
               :Spannung "Voltage"
               :Stromstärke "Current"
               :Temperatur "Temperature"
               :Umdrehungen "Rotations"
               :Volumen "Volume"
               :Volumenstrom "Volumetric flow rate"
               :Anomaliemodelle "Anomaly models"
               :Leistungerwartungen "Expected power"}
        :charts {:select-hint "Please select WECs above the tree view."
                 :grey "grey"
                 :negative "negative"
                 :logarithmic "logarithmic"
                 :error-table-header "Error messages SCADA"
                 :settings-table-header "Settings changes"
                 :modes-table-header "Operation modes"
                 :error-tooltip "Visualize the error periods in the chart."
                 :modes-tooltip "Visualize all operation modes, only possible if exactly one WEC is selected"
                 :parameters "Parameters"
                 :parameters-tooltip "Show all manual settings changes."
                 :production "Production"
                 :production-tooltip "Only show data for periods where the WEC was in a production mode. This data is the only one processed by the anomaly detection algorithms."
                 :compare "Compare"
                 :compare-tooltip "Scale each sensor series such, that they fill the vertical space of the chart. This makes comparison of sensors with different magnitudes easier."
                 :average "Averages"
                 :average-tooltip "Only show averages, hide the lighter min/max visualization."
                 :reload "Reload necessary!"
                 :zoom-setting "Mouse wheel zoom"
                 :zoom-tooltip "Zoom/Pan like Google Maps"}
        :powercurve {:date-range "Overall date range (%1 - %2)"
                     :learn-end "End of learning (%1)"
                     :learn-duration "Learning date range"
                     :learn-settings "Settings"
                     :threshold "Out of range power threshold for learning"
                     :confidence "Confidence interval"
                     :half-life "Half-time of sensor data"
                     :ignore-spikes? "Ignore singular power deviations"
                     :outliers "outliers"
                     :submit "Apply"}}
   
   :de {:missing "!!! Fehlender Text !!!"
        :please-wait "Bitte warten...."
        :all "Alle"
        :none "Keine"
        :start "Start"
        :end "Ende"                                   
        :wind-rose-tooltip "%1% der Zeit aus %2° mit \u2205 %3m/s"
        :loading "Lade..."
        :time {:duration "Dauer"
               :period "Zeitraum"
               :day "Tag"
               :days "Tage"
               :hour "Stunde"
               :hours "Stunden"
               :minute "Minute"
               :minutes "Minuten"
               :second "Sekunde"
               :seconds "Sekunden"
               :weekday-short #(nth ["Mo" "Di" "Mi" "Do" "Fr" "Sa" "So"] (first %1))
               :weekday-long #(nth ["Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag" "Sonntag"] (first %))
               :months (fn [] ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"])
               :months-long #(nth ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"] (first %1))}
        :titles {:availability "Verfügbarkeit"
                 :scadadata "Betriebsdaten"
                 :anomalies "Anomalien"
                 :sensors "Sensoren"
                 :wind "Windrosen"
                 :powercurve "Leistungskennlinie"}
        :modes-page {:choose "Bitte wählen Sie eine Anlage aus:"
                     :mode "Betriebsmodus"
                     :durations "Verweilzeiten in Betriebszustand"
                     :per-day "\"%1\" pro Tag"
                     :color-scale "Farbskala"
                     :legend "Legende"
                     :title "Gesamtübersicht pro Tag"
                     :errors "Fehlermeldungen"
                     :no-errors "keine Fehler"}
        :wind-page {:title "Verteilung von Windrichtung und -geschwindigkeit"
                    :angle "Sektorenwinkel:"
                    :apply "Anwenden"
                    :instructions "Bitte wählen Sie eine Anlage aus um eine Detailansicht zu sehen."}
        :anomalies {:button-tooltip "Anzeige als interaktiver Chart"
                    :deviation-tooltip "Verschiebung des Wertes um x Standardabweichungen vom Durchschnitt des Lernzeitraums"
                    :deviation-title "Verschiebung"
                    :sdr-tooltip "Verhältnis der Standardabweichung der jüngsten Vergangenheit zur SD des Lernzeitraums"
                    :target "Modellziel"
                    :target-tooltip "Regressionsmodell für diesen Sensor"
                    :predictors "Modellprädiktoren"
                    :sensor-name "Sensorname"
                    :too-high "zu hoch"
                    :too-low "zu niedrig"
                    :duration-in-day "Dauer am Tag"}
        :tree {:Kraft "Drehmoment"}
        :powercurve {:date-range "Gesamtzeitraum (%1 - %2)"
                     :learn-end "Ende des Lernens (%1)"}
        "powerprediction" "Erwartete vs. erzeugte Leistung"
        "powerlosses" "Leistungsverlust"}})

(reg-sub :i18n/language (fn [db] (db :language :de)))

(reg-event-db :i18n/select-language
              (fn [db [_ lang]]
                (assoc db :language lang)))

(defn i18n
  ([vs] (i18n vs nil))
  ([vs args] 
   (let [lang @(subscribe [:i18n/language])]
     (tempura/tr {:dict dict
                  :default-locale lang}
                 [lang]
                 vs
                 args))))
