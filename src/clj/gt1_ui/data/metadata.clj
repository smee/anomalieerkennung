(ns gt1-ui.data.metadata
  (:require [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as zx]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [clojure.java.io :as io]))

(def physical-entities
  {"kNm" "Kraft"
   "rad/s" "???"
   "kW" "Leistung"
   "kvar" "Blindleistung"
   "V" "Spannung"
   "A" "Stromstärke"
   "Hz" "Frequenz"
   "bar" "Druck"
   "DL" "???"
   "°C" "Temperatur"
   "°" "Ausrichtung"
   "rpm" "Umdrehungen"
   "kW/h" "Energie"
   "Pa" "Druck"
   "%" "Anteil"
   "L" "Volumen"
   "L/min" "Volumenstrom"
   "inc" "???"
   "mm" "Länge"
   "1/min" "Umdrehungen"
   "rpm/s" "Umdrehungen"
   "l/h" "Volumenstrom"
   "m/s" "Geschwindigkeit"
   "m/s²" "Beschleunigung"
   "°/s" "Rotationsgeschwindigkeit"})

(defn parse-structure [file]
  (let [s (z/xml-zip (x/parse (io/as-file file)))]
    (vec (for [mc (zx/xml-> s :MainComponents :MainComponent)
           :let [mc-name (zx/attr mc :Name)
                 mc-id (zx/attr mc :Type)]]
       {:index mc-id
        :label mc-name
        :children (vec
                   (for [c (zx/xml-> mc :Components :Component)
                         :let [id (zx/attr c :Type)]]
                     {:label (zx/attr c :Name)
                      :index id
                      :children (vec
                                 (for [{:keys [Name Number Unit Description]} (map :attrs (zx/xml-> c :TenMinutesAverageChannels :AnalogValueTemplate z/node))]
                                   {:label Name
                                    :index Number
                                    :unit Unit
                                    :dimension (get physical-entities Unit "???")
                                    :description Description}))}))}))))

(defn parse-status-codes [file]
  (let [s (z/xml-zip (x/parse (io/as-file file)))]
    (into {}
          (for [c (zx/xml-> s :MainComponents :MainComponent :Components :Component)
                   :let [id (zx/attr c :Type)]]
               [(read-string id)
                {:name (zx/attr c :Name)
                 :codes (into (sorted-map)
                              (for [{:keys [Name Number Code ShortCode Description]} (map :attrs (zx/xml-> c :StatusCodeTemplates :ChannelTemplate z/node))
                                    :let [id (read-string Number)]]
                                [id {:label Name
                                     :id id
                                     :description Description
                                     :code Code
                                     :short-code ShortCode}]))}]))))

(comment
  (require '[clojure.inspector :refer [inspect-tree]])
  (inspect-tree (parse-structure "/home/steffen/daten/gt1/metadaten/M5000-de.xml"))
  (inspect-tree (parse-status-codes "/home/steffen/daten/gt1/metadaten/M5000-de.xml"))
  )
