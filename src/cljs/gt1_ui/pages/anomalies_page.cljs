(ns gt1-ui.pages.anomalies-page
  (:require [reagent.session :as session]
            [reagent.ratom :refer-macros [reaction run!]]
            [reagent.format :refer [format]]            
            [gt1-ui.remote :as remote]
            [gt1-ui.pages.common :as pc :refer [at]]
            [accountant.core :as accountant]
            [gt1-ui.util.tree :as tree]
            [cljs-time.core :as time]
            [cljs-time.format :as tf]
            [historian.core :as hist :refer-macros [with-single-record]]
            [clojure.string :as str]
            [reagent.core :as reagent]))

(def name->label* (reaction
                   (->> :series
                        (tree/by-attribute (session/get-in [:structure :tree]))
                        (map (fn [[k v]] [k (:label v)]))
                        (into {}))))
(defn series-prefix [s]
  (let [idx (str/index-of s "|")]
    (if idx (subs s 0 idx) s)))

(defn series-parts [s]
  (str/split s "+"))

(defn name->label [s]
  (str/join "+" (map (fn [s] (@name->label* (series-prefix s))) (series-parts s))))

(defn show-charts!
"Change global application state to switch to a chart view of `sensors`."
  [timestamp since plant sensors]
  (let [start-date (time/minus since (time/days 12))
        end-date (time/plus timestamp (time/days 2))
        series-names (set (mapcat series-parts sensors))
        selection (->> :id
                       (tree/by-attribute (session/get-in [:structure :tree]))
                       (filter (fn [[_ m]] (contains? series-names (:series m))))
                       (into {}))] 
    (hist/with-single-record
      ;; select plant
      (session/update! :plants (fn [ps] (vec (for [p ps]
                                              (assoc p :selected? (= (:name p) plant))))))
      ;; select date range
      (session/assoc-in! [:chart :overall] {:start-date {:year (time/year start-date)
                                                         :month (time/month start-date)
                                                         :day (time/day start-date)}
                                            :end-date {:year (time/year end-date)
                                                       :month (time/month end-date)
                                                       :day (time/day end-date)}})
      ;; remove zoom
      (session/assoc-in! [:chart :zoom] nil))
    (session/assoc-in! [:tree :active :selected] selection)
    ;; switch view
    (accountant/navigate! "/")))

(defn short-number [float]
  (format "%.3f" float))

(defn percent [float]
  (format "%.1f%%" (* 100 float)))

(defn chart-button [event-timestamp start-time current-plant sensors]
  (let [label (if (= 1 (count sensors)) "Chart" "Charts")]
    [:button {:class "btn btn-default"
              :title "Anzeige als interaktiver Chart"
              :on-click #(show-charts! event-timestamp start-time current-plant (mapv series-prefix sensors))}
     label]))

(defn models-table [models timestamp current-plant anomalous?]
  [:table.table
   [:thead
    [:tr
     [:th {:title "Regressionsmodell für diesen Sensor"} "Modellziel"]
     [:th {:title "Divergenz Modell/Sensor in Prozent des Wertebereichs im Lernzeitraum"} "Abweichung"]
     [:th {:title "Verhältnis der Standardabweichung der jüngsten Vergangenheit zur SD des Lernzeitraums"} [:span "SD" [:sub "ratio"]]]
     [:th {:title "Verschiebung des Wertes um x Standardabweichungen vom Durchschnitt des Lernzeitraums"} [:span "Verschiebung?"]]
     [:th {:title "Name des Prädiktors des Regressionsmodells"} "Sensorname"]
     [:th {:title "Korrelation des Wertes des Prädiktors mit dem Residual von Modellausgabe und Messwert"} "Korrelation"]
     [:th {:title "Verhältnis der Standardabweichung der jüngsten Vergangenheit zur SD des Lernzeitraums"} [:span "SD" [:sub "ratio"]]]
     [:th {:title "Verschiebung des Wertes um x Standardabweichungen vom Durchschnitt des Lernzeitraums"} [:span "Verschiebung?"]]]]
   [:tbody
    (doall
     (for [[target {sdt :sd-ratio preds :predictors r :residual d :deviation}] (sort-by first models)
           :let [rows (count preds)
                 preds (if (empty? preds) {nil {:correlation 0 :sd-ratio 1.0 :deviation 0.0}} preds)]
           [idx p {sdp :sd-ratio corr :correlation dp :deviation}] (map-indexed (fn [idx [k v]] [idx k v]) (reverse (sort-by (comp :correlation second) preds)))
           :let [first-row? (zero? idx)
                 pred-names (keep identity (keys preds))]]       
       (cond-> ^{:key (str target idx)} [:tr]
         first-row? (into [[:td {:rowSpan rows
                                 :title target
                                 :class (when (anomalous? target) "anomaly")}
                            (or (name->label target) target)
                            (chart-button timestamp timestamp current-plant [target])
                            (when (not-empty pred-names) (chart-button timestamp timestamp current-plant (cons target pred-names)))]
                           [:td {:rowSpan rows} (percent r)]
                           [:td {:rowSpan rows} (percent sdt)]
                           [:td {:rowSpan rows} (short-number d)]])
         (not (nil? p)) (into [[:td {:title p
                                     :class (when (anomalous? p) "anomaly")}
                                (or (name->label p) p) (chart-button timestamp timestamp current-plant [p])]
                               [:td (short-number corr)]
                               [:td (percent sdp)]
                               [:td (short-number dp)]]))))]])

(defn- store-annotation! [timestamp plant text]
  (remote/POST "/db/anomalies/annotations" {:params {:plant plant :timestamp (tf/unparse (tf/formatters :basic-date) timestamp) :text text}}))


(defn anomalies-page [current-plant]
  (let [atm (at :anomalies)
        internal (at :anomaly-internal)
        plants (reaction (mapv :name (session/get :plants)))
        toggle! (fn [key timestamp] (swap! internal update-in [key] (fn [ts] (when (not= ts timestamp) timestamp))))]
    ;; remember scroll position on umount
    ;; TODO as mixin? make reusable!
    (reagent/create-class
     {:component-did-mount (fn [] (js/window.scrollTo 0 (session/get-in [:anomalies-page :scroll] 0)))
      :component-will-unmount #(session/assoc-in! [:anomalies-page :scroll] @pc/cur-scroll-y)
      :reagent-render
      (fn [current-plant]
        ;; load new data after each selection change 
        (when (and current-plant (string? current-plant) (not= current-plant (:selected @internal)))
          (remote/GET "/db/anomalies"
              {:params {:plant current-plant}
               :handler #(reset! atm %)})
          (remote/GET (str "/db/anomalies/annotations/" current-plant)
              {:handler #(swap! internal assoc :annotations %)})
          (swap! internal assoc :selected current-plant)
          (reset! atm []))
        (let [dd (:details-for @internal)
              ann (:annotations-for @internal)
              current-plant (:selected @internal)]
          [:div
           [pc/toolbar]
           [pc/loading-bar]
           [:h2 "Anomalien"]
           [:select {:default-value current-plant
                     :value current-plant
                     :on-change #(accountant/navigate! (str "/anomalies/" (.. % -target -value)))}
            [:option {:value "n/a"} "Auswählen"]
            (for [plant @plants]
              ^{:key plant} [:option {:value plant} plant])]
           [:button {:class "button btn-default"
                     :on-click #(let [prev (or (ffirst (filter (fn [[_ p]] (= current-plant p)) (partition 2 1 @plants)))
                                               (last @plants))]
                                  (accountant/navigate! (str "/anomalies/" prev)))} "<<"]
           [:button {:class "button btn-default"
                     :on-click #(let [next (or (second (first (filter (fn [[p]] (= current-plant p)) (partition 2 1 @plants))))
                                               (first @plants))]
                                  (accountant/navigate! (str "/anomalies/" next)))} ">>"]
           [:table {:class "table"}
            [:colgroup
             [:col {:style {:width "30%"}}]
             [:col {:style {:width "10%"}}]
             [:col {:style {:width "50%"}}]
             [:col {:style {:width "10%"}}]]
            [:thead
             [:tr
              [:th "Datum"]
              [:th "Seit"]
              [:th "Sensor"]
              [:th "Anzahl Hinweise"]]]
            (reduce into [:tbody]
                    (for [{timestamp :timestamp {:keys [anomalies candidates]} :results} (reverse @atm)
                          :let [anomalous (keys anomalies)
                                anomalous? (fn [s] (some #(when s (.startsWith s %)) anomalous))
                                show-models? (= timestamp dd)
                                show-annotations? (= timestamp ann)
                                annotation-text (get-in @internal [:annotations timestamp])
                                edit-classes "btn btn-default glyphicon glyphicon-pencil"
                                edit-classes (if annotation-text (str edit-classes " btn-info") edit-classes)
                                first-col [:td {:rowSpan (+ (count anomalies) (if show-models? 1 0))}
                                           [:div.container-fluid ;{:style {:min-width 200}}
                                            [:div.row (pc/human-date timestamp)
                                             [:button.btn.btn-default {:class (when show-models? "active")
                                                                       :title "Detailansicht der auffälligen Regressionsmodelle"
                                                                       :on-click #(do (toggle! :details-for timestamp)
                                                                                      (.stopPropagation %))} "Modelle"]
                                             [:button {:class (if show-annotations? (str edit-classes " active") edit-classes)
                                                       :title "Anmerkungen zur Evaluierung"
                                                       :on-click #(do (toggle! :annotations-for timestamp)
                                                                      (.stopPropagation %))}]]
                                            (when show-annotations?
                                              [:div.row
                                               [:div.form-group [:textarea {:default-value annotation-text}]]
                                               [:button {:class "btn btn-default"
                                                         :type "submit"
                                                         :on-click #(let [text (-> (.getElementsByTagName (.-parentNode (.-target %)) "textarea") (aget 0) .-value)]
                                                                      (store-annotation! timestamp current-plant text)
                                                                      (swap! internal assoc-in [:annotations timestamp] (when (not-empty text) text))
                                                                      (toggle! :annotations-for timestamp)
                                                                      (.stopPropagation %))}
                                                "Speichern"]])]]]
                          ;; TODO show all candidates even if we could not identify a clear reason
                          [idx sensor {since :since n :n}] (map-indexed (fn [idx [k v]] [idx k v]) (reverse (sort-by :n anomalies)))
                          :let [key (str idx timestamp sensor)
                                duration-in-days (time/in-days (time/interval since timestamp))]] 
                      [;; if we toggled the button, insert detail table
                       (when (and show-models? (zero? idx))
                         ^{:key (str key "model")}
                         [:tr first-col
                          [:td {:col-span 6}
                           [models-table candidates timestamp current-plant anomalous?]]])
                       ;; default table rows
                       ^{:key key}
                       [:tr 
                        (when (and (not show-models?) (zero? idx))
                          first-col)
                        [:td {:title (pc/human-date since)} (if (zero? duration-in-days) [:b "NEU"] (str duration-in-days " Tagen"))]
                        [:td {:title sensor} (or (name->label sensor) sensor) (chart-button timestamp since current-plant [sensor])]
                        [:td n]]]))]
           [:button.btn.btn-default {:on-click #(remote/POST "/process" {})
                                     :style {:display "none"}} "Erkennung starten"]]))})))
