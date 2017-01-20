(ns scada-ui.data.structure
  (:require [scada-ui.util.tree :as tree]
            [org.clojars.smee.map :refer [map-values]]
            [scada-ui.util :refer [md5]]
            [hasch.core :as h]))

(defonce ^:private cache (atom {:specs {} :plants {}}))

(defn fill-cache! [specs plants]
  {:pre [(map? specs) (vector? plants)]}
  (do
    (reset! cache {:specs (map-values #(let [m (assoc % :sensors (tree/by-attribute (:structure %) :series))
                                             hash (str (h/uuid m))]
                                         (assoc m :hash hash)) specs)
                   :plants (->> plants (group-by :name) (map-values first))})
    nil))

(defn plant-spec-versions []
  (map-values :hash (get @cache :specs)))

(defn plant-spec [name] (get-in @cache [:specs name]))

(defn plant-spec-of [plant]
  (let [{:keys [specs plants]} @cache]
    (->> plant (get plants) :type (get specs))))

(defn plants []
  (vals (:plants @cache)))

(defn find-trace-name [spec-name sensor-id]
  (get-in @cache [:specs spec-name :sensors sensor-id :trace]))

(defn sensor-type [plant sensor]
  (-> plant
      plant-spec-of
      :sensors
      (get sensor)
      :point-type))

(defn wind-speed-sensor-name [plant] (-> plant plant-spec-of :functions :wind :speed))

(defn power-sensor-name [plant] (-> plant plant-spec-of :functions :power))

(defn target-sensor-names [plant] (-> plant plant-spec-of :functions :targets))

(defn production-column [plant] (-> plant plant-spec-of :status :production-sensor (str "/avg")))

(defn max-power [plant] (-> plant plant-spec-of :max-power))
