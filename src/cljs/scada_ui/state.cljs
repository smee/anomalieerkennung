(ns scada-ui.state
  (:require [clojure.string :as str]
            [clojure.data :refer [diff]]
            [cljs-time.core :as time]
            [re-frame.core :as re :refer [reg-event-db reg-event-fx path dispatch reg-fx subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :refer [run!]]
            scada-ui.components.tree
            [scada-ui.format :as f]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.util.tree :as tree]
            [cljs-time.format :as tf]))

(def today (tf/parse (tf/formatter "yyyy-MM-dd") (.-textContent (.getElementById js/document "today"))))
;; ------------ simple helper functions for re-frame handlers and subscriptions, reduces boilerplate ---------------

(defn assoc-in-handler
"Register trivial direct writing handler, works like `(assoc-in app-db keys name)`"
  [name keys]
  (reg-event-db name (path keys) (fn [_ [_ value]] value)))

(defn toggle-handler
"Register trivial handler to toggle boolean values, works like `(update-in app-db keys not)`"
  [name keys]
  (reg-event-db name (path keys) (fn [value [tag]] (not value))))

(defn reg-sub
"Remove boilerplate for common pattern where a specific fixed path is selected in the application db."
  ([name path] (reg-sub name path identity))
  ([name path f] (re/reg-sub name (fn [db] (f (get-in db path))))))

(defn cursor
  ([subscription dispatch-v] (cursor subscription nil dispatch-v))
  ([subscription subs-keys dispatch-v]
   (reagent/cursor (fn ([_k] (if subs-keys
                              (get-in (deref subscription) subs-keys)
                              (deref subscription)))
                     ([_k v] (do (dispatch (into dispatch-v [v])))))
                   nil)))


(defn series-prefix [s]
  (let [idx (str/index-of s "|")]
    (if idx (subs s 0 idx) s)))

(defn series-suffix [s]
  (let [idx (str/index-of s "|")]
    (if idx (subs s (inc idx)) "")))

(defn series-parts [s]
  (let [suffix (series-suffix s)]
    (str/split s "+")))



;;;;;;;;  all plants, common subscriptions/handlers for selecting plants, plant metadata etc ;;;;;;;;;;;;;;;;;;;;
(reg-sub :plants [:plants])
(re/reg-sub :plant-coordinates :<- [:plants] #(mapv (juxt :latitude :longitude) %))
(re/reg-sub :selected-plants :<- [:plants] #(vec (filter :selected? %)))
(re/reg-sub :all-plant-names :<- [:plants] #(mapv :name %))
(re/reg-sub :selected-plant-names :<- [:selected-plants] #(mapv :name %))

(reg-event-db :toggle-plant (path :plants)
              (fn [plants [_ idx]]
                (let [idx (if (number? idx)
                            idx
                            (first (keep-indexed #(when (= idx (:name %2)) %1) plants)))]
                  (update-in plants [idx :selected?] not))))

(reg-event-db :select-exclusively-plants (path :plants)
              (fn [plants [_ names]]
                (let [enabled? (comp boolean (set names))]
                  (mapv #(assoc % :selected? (enabled? (:name %))) plants))))

(reg-event-db :select-all-plants (path :plants)
              (fn [plants [_ select?]]
                (mapv #(assoc % :selected? select?) plants)))

(reg-event-db :select-plant (path :plants)
              (fn [plants [_ name]]
                (mapv #(if (= name (:name %))
                         (assoc % :selected? true)
                         %) plants)))

;; return map that gives event details per id
(reg-sub :structure-name [:structure :name])
(reg-sub :event-details [:structure :events])
(reg-sub :change-details [:structure :parameters])

(reg-sub :operation-modes [:structure :status :values])
(reg-sub :production-modes [:structure :status :production-codes])
(reg-sub :sampling-rate [:structure :sampling-rate])
(reg-sub :sensor-tree [:structure :structure])

;; select nodes in tree, plants, set overall date, remove zoom
(reg-event-db :configure-chart
              (fn [db [_ {:keys [series-ids plants dates]}]]
                (let [selected? (set plants)
                      {:keys [start end]} dates]
                  (-> db
                      (update-in [:plants] (partial mapv #(assoc % :selected? (boolean (selected? (:name %))))))
                      (assoc-in [:tree :selected?] (set series-ids))
                      (assoc-in [:chart :overall] {:start start :end end})
                      (assoc-in [:chart :zoom] {:start nil :end nil})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; currently selected sensor/component tree
(def default-power-config {:threshold 100
                           :half-life (* 24 6 10)
                           :confidence 0.99
                           :ignore-spikes? true
                           :learn-end (time/date-time 2016 6 1)
                           :dates {:start (time/date-time 2016) :end today}})

(reg-sub :powercurves [:power-curve :configurations])

(reg-event-db :create-default-powercurve-configs
  (fn [db]
    (assoc-in db [:power-curve :configurations]
      (reduce
       (fn [res {plant :name}]
         (assoc res plant [default-power-config]))
       (sorted-map) (:plants db)))))

(reg-event-fx :add-powercurve-config
              (fn [{db :db} [_ plant config]]
                (let [db' (update-in db [:power-curve :configurations plant] (fnil conj []) config)]
                  {:db db'}))) ;; TODO select in tree

(defn derive-powercurve-label [config]
  (let [[_ changed] (diff default-power-config config)]
    (if (empty? changed)
      "Leistungskennlinie"
      (f/format "Lkl (%s)"
                (str/join ", " (for [[k v] changed]
                                 (case k
                                   :learn-end (str (i18n [:powercurve/learn-end "Lernende"] [(f/human-date v)]))
                                   :confidence (f/format "%s %f%%" (i18n [:powercurve/confidence "Bandbreite"]) (* v 100))
                                   :threshold (str (i18n [:powercurve/threshold "max. Abweichung"]) " " v "kW")
                                   :ignore-spikes? (str (if v (i18n [:with "mit"]) (i18n [:without "ohne"])) " " (i18n [:powercurve/outliers "Ausreißer"]))
                                   :dates (when (:start v) (str (i18n [:from "ab"]) " "(f/human-date (:start v))))
                                   :half-life (str (i18n [:powercurve/half-life "Halbwertszeit"]) " " v))))))))

(re/reg-sub :power-subtree
            :<- [:selected-plant-names]
            :<- [:powercurves]
            (fn [[plants pc-configs]]
              {:label "Leistungerwartungen"
               :id "powercurves"
               :children (vec
                          (for [p plants]
                            {:label p
                             :id (str "power_" p)
                             :type :power
                             :plant p
                             :children (vec
                                        (for [[idx config] (map vector (range) (get pc-configs p))
                                              :let [label (derive-powercurve-label config)]]
                                          {:id (str "power_" p "_" idx)
                                           :label label
                                           :type :power
                                           :plant p
                                           :options config}))}))}))

(reg-sub :anomaly-models [:anomalies-page :algorithms])

(re/reg-sub :models-subtree
            :<- [:name->label]
            :<- [:anomaly-models]
            :<- [:sensor-tree]
            (fn [[lbl-fn lasso-models sensor-tree]]
              {:label "Anomaliemodelle"
               :id "lasso-models"
               :type :lasso-config
               :button {:contents [:i {:class "glyphicon glyphicon-option-horizontal"
                                       :title (i18n [:tree/load-models "Modelle laden"])}]
                        :dispatch-v [:anomalies/load-full-algorithms]}
               :children
               (if (empty? lasso-models) []
                 (vec
                  (for [[plant models] lasso-models]
                    {:label plant
                     :id (str "lasso-models " plant)
                     :type :lasso-config
                     :children (vec
                                (for [{:keys [id plant learn-start learn-end algorithm options]} (sort-by :learn-start models)
                                      :let [options (assoc options :dates {:start learn-start :end learn-end}) 
                                            dates (str (f/date->str learn-start) " - " (f/date->str learn-end))]]
                                  {:label dates
                                   :id id
                                   :type :lasso-config
                                   :options options
                                   :children (let [by-series (group-by #(series-prefix (str/replace % "/avg" "")) (keys algorithm))]
                                               (clojure.walk/postwalk
                                                (fn [x]
                                                  (cond
                                                    (and (map? x) (= :sensor (:type x)))
                                                    (if-let [model-names (by-series (:series x))]
                                                      (assoc x :children (vec
                                                                          (for [k (sort model-names)
                                                                                :let [{{:keys [attributes intercept r-squared]} :parameters} (get algorithm k)
                                                                                      suffix (series-suffix k)
                                                                                      label (lbl-fn (str/replace (series-prefix k) "/avg" ""))
                                                                                      id' (str k "|" id)]]
                                                                            {:id id'
                                                                             :label (str (if label (str label "|" suffix) k))
                                                                             :series id'
                                                                             :target k
                                                                             :algorithm id
                                                                             :plant plant
                                                                             :type :lasso
                                                                             :attributes attributes})))
                                                      nil)
                                                    (map? x) (assoc x :id (tree/random-id))
                                                    ;; we want 'real' vectors, not map kv-pairs
                                                    (and (vector? x) (every? (some-fn nil? map?) x)) (vec (remove nil? x))
                                                    :else x))
                                                sensor-tree))}))})))}))

(re/reg-sub :tree
            :<- [:sensor-tree]
            :<- [:power-subtree]
            :<- [:models-subtree]
            (fn [[main-tree power-subtree models-subtree]]
              (cond-> main-tree
                power-subtree  (conj power-subtree)
                models-subtree (conj models-subtree))))

(re/reg-sub :all-sensors :<- [:sensor-tree]
            (fn [tree]
              (->> :series
                   (tree/by-attribute tree)
                   vals
                   (filter #(= :sensor (:type %)))
                   (sort-by :label))))

;;;;;;;;;;;;;;; series handling, components ;;;;;;;;;;;;;;;;;;;;;;;;

(re/reg-sub :db (fn [db] db))
(re/reg-sub :by-series :<- [:sensor-tree] (fn [tree] (tree/by-attribute tree :series)))
(re/reg-sub :by-id :<- [:tree] (fn [tree] (tree/by-attribute tree :id)))

(re/reg-sub :id->node :<- [:by-id]
            (fn [by-id]
              (fn [id]
                (when id
                  (if-let [res (by-id id)]
                    res
                    (let [parts (series-parts id)]
                      (if (= 1 (count parts))
                        (by-id (first parts))
                        (let [nodes (map by-id parts)
                              attr (fn [key] (str/join "/" (distinct (map key nodes))))
                              types (distinct (map :type nodes))]
                          {:series id
                           :id id
                           :label (attr :label)
                           :type (when (= 1 (count types)) (first types)) ;; we need a singular type, can't handle combination of different types
                           :unit (attr :unit)
                           :dimension (attr :dimension)
                           :description (attr :description)}))))))))

;;{series name of a sensor, sensor label}
(re/reg-sub :name->label :<- [:by-series]
            (fn [by-series]
              (let [m (->> by-series
                           (map (fn [[k v]] [k (:label v)]))
                           (into {}))]
                (fn [series]
                  (when series
                    (str/join "+"
                              (map (fn [s]
                                     (let [prefix (series-prefix s)
                                           lbl (m prefix s)]
                                       (i18n [lbl lbl])))
                                   (series-parts series))))))))

;; {series name of a sensor, unique id for tree selection}
(re/reg-sub :series->id :<- [:series->node]
            (fn [series->node-fn]
              (fn [series]
                (:id (series->node-fn series)))))

;; {component id, component label}
(re/reg-sub :component->label :<- [:sensor-tree]
            (fn [tree]
              (reduce-kv (fn [res c {lbl :label}]
                           (assoc res c lbl))
                         {} (tree/by-attribute tree :component))))


(re/reg-sub :selected-series 
            :<- [:selected-plants]
            :<- [:id->node]
            :<- [:tree/selected-ids]
            (fn [[plants id->node selected?]]
              (let [nodes (mapv id->node selected?)
                    plants (mapv :name plants)
                    start (time/date-time 2016 1 1)
                    {sensors :sensor, models :lasso, power :power} (group-by :type nodes)
                    sensors (for [p plants
                                  s (mapv :series sensors)
                                  :let [type (:type s)]
                                  :when (nil? type)]
                              {:plant p :series s :type :sensor})                    
                    models (mapv (fn [{:keys [algorithm target plant]}]
                                   {:series target
                                    :algorithm algorithm
                                    :plant plant
                                    :type :lasso
                                    :start start
                                    :end today}) models)
                    power (for [m power]
                            (-> m
                                (select-keys [:plant :type :options])
                                (assoc :start start :end today)))]
                (vec (concat sensors models power)))))

;; all events (errors, changes) are missing details, need to be enriched
(re/reg-sub :hydrated-errors
     (fn [[_ q-v]]
       {:pre [(vector? q-v)]}
       [(subscribe [:component->label])
        (subscribe [:event-details])
        (subscribe q-v)])
     (fn [[c->lbl details-fn events]]
       (mapv (fn [event]
               (update (merge event (details-fn (:id event))) :component c->lbl))
             events)))

