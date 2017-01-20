(ns scada-ui.pages.anomalies-page
  (:require [reagent.format :refer [format]]            
            [scada-ui.pages.common :as pc]
            [scada-ui.format :as f]
            [scada-ui.components.bootstrap :as bs]
            ;;[scada-ui.components.anomalies :refer [options-form]]
            [scada-ui.state :as state]
            [scada-ui.components.date-picker :refer [date-range-picker]]
            [scada-ui.util :refer [map-values]]
            [scada-ui.i18n :refer [i18n]]
            [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [cljs-time.format :as tf]
            [clojure.string :as str]
            [re-frame-datatable.core :as dt]
            [reagent.core :as reagent]
            [re-frame.core :refer [reg-event-db reg-event-fx reg-sub path dispatch subscribe debug]]
            [cljs-time.coerce :as tc]
            [clojure.set :as set]
            [scada-ui.components.table :as tables]))


(defn- series->sensor [s]
  (let [s' (state/series-prefix s)]
    (cond-> s'
      (str/includes? s' "/avg") (str/replace  "/avg" "")
      (str/includes? s' "/min") (str/replace  "/min" "")
      (str/includes? s' "/max") (str/replace  "/max" ""))))

;;;;;;;;;;;;;;;; logic;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def default-state {:current {:plant nil ;; nil if we show multiple plants (most recent day) or a single plants history
                              :anomalies {} ;; {plant [results]}
                              :details-for nil
                              :active-tab "lasso"}
                    :configs {:available nil ;; {plant [{:keys [learn-end id ...]}]}
                              :by-id {}
                              :current nil ;; {plant id}
                              }
                    :dates {:detail {:start (time/minus state/today (time/months 1))
                                     :end state/today} ;; date range for single plant results
                            :overview {:start (time/minus state/today (time/days 1))
                                       :end state/today} ;; date range for multiple plants
                            }
                    :|n| 3  ;; min. number of hints to show (lasso-model specific)                
                    :all-candidates? false ;; same as setting |n| to 1
                    })

(def middleware [#_debug (path [:anomalies-page])])

(state/assoc-in-handler :anomalies/active-tab [:anomalies-page :current :active-tab])
(state/reg-sub :anomalies/active-tab [:anomalies-page :current :active-tab])

(reg-event-db :anomalies/received-anomalies middleware
              (fn [db [_ anomalies]]
                (assoc-in db [:current :anomalies] (group-by :plant (map #(assoc % :type (-> db :configs :by-id (get (:algorithm %)) :type)) anomalies)))))

(state/assoc-in-handler :anomalies/minimum-indications [:anomalies-page :|n|])


(reg-event-fx :anomalies/received-configs middleware
              (fn [{{{plant :plant} :current :as db} :db} [_ algs]]
                (let [{lasso-models "lasso" thresholds "M5000"} (group-by :type algs)
                      by-plant (map-values (comp vec (partial sort-by :learn-end)) (group-by :plant lasso-models))
                      current (get-in db [:configs :current])
                      ;; select newest lasso model per plant
                      newest (map-values (fn [vs] (:id (apply (partial max-key :learn-end) vs))) by-plant)
                      ;; TODO if currently selected id is not present anymore, select the most recent one
                      current (or (get-in db [:configs :current]) newest) 
                      thr (map-values (comp :id first) (group-by :plant thresholds))] 
                  {:db (assoc db :configs {:available by-plant
                                           :current current
                                           :thresholds thr
                                           :by-id (map-values first (group-by :id algs))})
                   :dispatch [:anomalies/load-results]})))

(reg-event-fx :anomalies/set-dates middleware
              (fn [{db :db} [_ dates]]
                (let [plant (-> db :current :plant)
                      k (if plant :detail :overview)]
                  {:db (assoc-in db [:dates k] dates)
                   :dispatch [:anomalies/load-results]})))

;;;;;;;; full lasso model details ;;;;;;;;;;;;;;;;;;;;;;;
;; load full details of LASSO anomaly models
;;

(reg-event-db :anomalies/received-algorithms
              (fn [db [_ results]]
                (update-in db [:anomalies-page :algorithms] merge (group-by :plant results))))

(reg-event-fx :anomalies/load-full-algorithms
              (fn [{db :db} [_]]
                (let [plants (->> db :plants (filter :selected?) (map :name) set)
                      present (-> db :anomalies-page :algorithms keys set)
                      missing (set/difference plants present)
                      to-remove (set/difference present plants)]
                  {:db (update-in db [:anomalies-page :algorithms] (fn [algs] (apply dissoc algs to-remove)))
                   :remote (when (not-empty missing)
                             {:url "/db/anomalies/algorithms"
                              :params {:plants (vec missing)}
                              :success [:anomalies/received-algorithms]})})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- find-by-id [available plant id]
  (first (filter #(= id (:id %1)) (get available plant))))

(reg-event-fx :anomalies/use-config middleware
              (fn [{{{:keys [available]} :configs :as db} :db} [_ plant id]]
                {:db (assoc-in db [:configs :current plant] id)
                 :dispatch [:anomalies/load-results]}))

(reg-event-fx :anomalies/use-configs middleware
              (fn [{{{:keys [current available]} :configs :as db} :db} [_ date-range]]
                (let [current' (reduce (fn [cur [plant models]]
                                         (if-let [{id :id} (first (filter #(= date-range (select-keys % [:learn-start :learn-end])) models))]
                                           (assoc cur plant id)
                                           cur))
                                       current available)]
                  {:db (assoc-in db [:configs :current] current')
                   :dispatch [:anomalies/load-results]})))

(reg-event-fx :anomalies/load-results middleware
              (fn [{{{plant :plant} :current
                    {:keys [detail overview]} :dates
                    {current :current thr :thresholds} :configs} :db} [_]]
                (let [many? (not plant)
                      ids (if many? (concat (vals current) (vals thr)) [(thr plant) (current plant)])
                      {:keys [start end]} (if many? overview detail)]
                 {:remote {:url "/db/anomalies/results"
                           :params {:dates [(f/date-time->str start) (f/date-time->str end)]
                                    :ids ids}
                           :success [:anomalies/received-anomalies]}})))

(reg-event-fx :anomalies/initialize middleware
  (fn [{{{old-plant :plant} :current {:keys [available current]} :configs :as db} :db} [_ new-plant]]
    (let [overview? (nil? new-plant)
          new? (and new-plant (not= old-plant new-plant))           
          alg-id (when current (current new-plant))]
      (cond-> {:db (assoc-in db [:current :plant] new-plant)
               :remote (cond-> []
                         (empty? available) (conj {:url "/db/anomalies/all-configs"
                                                   :success [:anomalies/received-configs]}))}        
        (and (not-empty available)
             (or (and new? alg-id)
                 (and overview? current))) (assoc :dispatch [:anomalies/load-results])))))

(reg-event-db :anomalies/toggle [middleware (path [:current])]
              (fn [db [_ key m']]
                (update-in db [key] (fn [m] (when (not= m m') m')))))

(state/toggle-handler :anomalies-page/toggle-candidates [:anomalies-page :all-candidates?])

(reg-event-fx :anomalies/show-charts [middleware]
              (fn [{:keys [db]} [_ timestamp since plant series-ids]]
                (let [start-date (time/minus since (time/days 12))
                      end-date (time/plus timestamp (time/days 2))]
                  {:navigate "/charts"
                   :dispatch-n [[:configure-chart {:series-ids series-ids
                                                   :plants [plant]
                                                   :dates {:start start-date :end end-date}}]
                                [:tree/focus-selected] ;; focus on selected items
                                [:tree/set-filter ""]]})))

;;;;;;;;;; subscriptions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(state/reg-sub :anomalies/state [:anomalies-page])
(state/reg-sub :anomalies/plant [:anomalies-page :current :plant])
(state/reg-sub :anomalies/minimum-indications [:anomalies-page :|n|])

;; distinct model ranges
(reg-sub :anomalies/model-date-ranges
         (fn [db] 
           (->> db :anomalies-page :configs :available vals
                (reduce into [])
                (map #(select-keys % [:learn-start :learn-end]))
                distinct
                (sort-by :learn-end) vec)))

(reg-sub :anomalies/date-range
         :<- [:anomalies/plant]
         :<- [:anomalies/state]
         (fn [[plant {{:keys [detail overview]} :dates}]]
           (if plant detail overview)))

(reg-sub :anomalies/view-data
         :<- [:anomalies/minimum-indications]
         :<- [:anomalies/plant]         
         :<- [:anomalies/date-range]
         :<- [:anomalies/state]
         (fn [[|n| plant date-range {{anomalies :anomalies :as data} :current {:keys [current available]} :configs show-all? :all-candidates?}]]
           (let [|n| (if show-all? 1 |n|)
                 rows (filter #(time/within? date-range (:timestamp %))
                              (if plant
                                (get anomalies plant)
                                (reduce into [] (vals anomalies))))
                 by-type (group-by :type rows)]
             (assoc data
                    :anomalies (->> (get by-type "lasso")                                    
                                    (map #(update-in % [:results :faults] (fn [m] (into {} (filter (fn [[k n]] (>= n |n|)) m)))))
                                    (sort-by :plant)
                                    reverse
                                    (sort-by :timestamp)
                                    reverse
                                    vec)
                    ;; TODO generic, by plant type!
                    :thresholds (get by-type "M5000")))))

(reg-sub :anomalies/threshold-violations
         :<- [:anomalies/view-data]
         :<- [:sampling-rate]
         (fn [[{tr :thresholds} sr-in-seconds]]
           (->>
            (for [{p :plant ts :timestamp {:keys [too-low too-high since]} :results} tr
                  :let [to-rows (fn [vs type]
                                       (mapv (fn [[sensor n]]
                                               {:type (i18n [(keyword (str "anomalies/" (name type)))])
                                                :plant p
                                                :timestamp ts
                                                :since (since sensor)
                                                :sensor sensor
                                                :duration (tables/duration-str (time/interval ts (time/plus ts (time/seconds (* sr-in-seconds n)))))}) vs))]]
              (into (to-rows too-low :too-low)
                    (to-rows too-high :too-high)))
            (reduce into [])
            (sort-by :plant)
            reverse
            (sort-by :timestamp)
            reverse
            vec)))

;;;;;;;;;;;;;;;; views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn short-number [float]
  (format "%.3f" float))

(defn percent [float]
  (format "%.1f%%" (* 100 float)))

(defn chart-button [event-timestamp start-time plant sensors]
  (let [label (if (= 1 (count sensors)) "Chart" "Charts")]
    [:button {:class "btn btn-default"
              :title (i18n [:anomalies/button-tooltip])
              :on-click (fn [_event]
                          (let [series-names (set (map series->sensor sensors))]
                            (dispatch [:anomalies/show-charts event-timestamp start-time plant series-names])))}
     label]))

(def deviation-hdr [:th {:title (i18n [:anomalies/deviation-tooltip])} [:span (i18n [:anomalies/deviation-title])]])
(def sdr-hdr [:th {:title (i18n [:anomalies/sdr-tooltip])} [:span "SD" [:sub "ratio"]]])

(defn sensor [sensor from to plant]
  (let [name->label (subscribe [:name->label])]
    (fn [series from to plant]
      [:span
       [:span {:title sensor} (or (@name->label (series->sensor sensor)) series)]
       [chart-button to from plant [series]]
       #_[:button.btn.btn-default {:title "Ausblenden"}
        [bs/Glyphicon {:glyph "eye-close"}]]])))

(defn plant-button [plant]
  [:button.btn.btn-default {:on-click #(dispatch [:navigate (str "/anomalies/" plant)])} plant])

(defn duration-in-days [since timestamp]
  (let [start (time/earliest since timestamp)
        end (time/latest since timestamp)
        duration-in-days (time/in-days (time/interval start end))]
    [:span {:title (f/human-date since)}
     (if (zero? duration-in-days)
       [:b (i18n [:new "NEU"])]
       (str duration-in-days " "(i18n [:time/days "Tagen"])))]))

(defn models-table [models timestamp plant anomalous?]
  [:table.table
   [:thead
    [:tr
     [:th {:colSpan 4} (i18n [:anomalies/target])] [:th {:colSpan 4} (i18n [:anomalies/predictors])]]
    [:tr
     [:th {:title (i18n [:anomalies/target-tooltip])} (i18n [:anomalies/sensor-name])]
     [:th {:title (i18n [:anomalies/divergence-tooltip "Divergenz Modell/Sensor in Prozent des Wertebereichs im Lernzeitraum"])} (i18n [:anomalies/divergence "Abweichung"])]
     sdr-hdr
     deviation-hdr
     [:th {:title (i18n [:anomalies/predictor-tooltip "Name des Prädiktors des Regressionsmodells"])} (i18n [:anomalies/sensor-name "Sensorname"])]
     [:th {:title (i18n [:anomalies/correlation-tooltip "Korrelation des Wertes des Prädiktors mit dem Residual von Modellausgabe und Messwert"])} (i18n [:anomalies/correlation "Korrelation"])]
     sdr-hdr
     [:th {:title (i18n [:anomalies/deviation-tooltip"Verschiebung des Wertes um x Standardabweichungen vom Durchschnitt des Lernzeitraums"])} (i18n [:anomalies/deviation"Verschiebung?"])]]]
   (into [:tbody]
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
                                [sensor target timestamp timestamp plant]
                                (when (not-empty pred-names) [chart-button timestamp timestamp plant (cons target pred-names)])]
                               [:td {:rowSpan rows :title r} (percent r)]
                               [:td {:rowSpan rows :title sdt} (percent sdt)]
                               [:td {:rowSpan rows :title d} (short-number d)]])
             (not (nil? p)) (into [[:td {:title p
                                         :class (when (anomalous? p) "anomaly")}
                                    [sensor p timestamp timestamp plant]]
                                   [:td {:title corr} (short-number corr)]
                                   [:td {:title sdp} (percent sdp)]
                                   [:td {:title dp} (short-number dp)]]))))])

(defn plant-selector [plants plant]
  [:div.form-inline {:style {:display "inline-block"}}
   [:div.form-group
    [:select.form-control
     {:value (or plant "Park")
      :title (i18n [:anomalies/select-wec "Auswahl der WEA"])
      :on-change #(let [idx (.. % -target -selectedIndex)]
                    (dispatch [:navigate (str "/anomalies" (when (pos? idx) (str "/" (nth @plants (dec idx)))))]))}
     (map-indexed (fn [i plant] ^{:key i} [:option {:value plant} plant]) (cons "Park" @plants))]] 
   [:button {:class "button btn-default"
             :on-click #(let [prev (or (ffirst (filter (fn [[_ p]] (= plant p)) (partition 2 1 @plants)))
                                       (last @plants))]
                          (dispatch [:navigate (str "/anomalies/" prev)]))} "<<"]
   [:button {:class "button btn-default"
             :on-click #(let [next (or (second (first (filter (fn [[p]] (= plant p)) (partition 2 1 @plants))))
                                       (first @plants))]
                          (dispatch [:navigate (str "/anomalies/" next)]))} ">>"]])


(defn- lbl-fn [{start :learn-start end :learn-end}]
  (when (and start end) (str (f/human-date start) " - " (f/human-date end))))

(defn algorithm-selector
"If there are multiple algorithms for a power plant, render a selector, else just a heading."
  [{:keys [current available]} plant]
  (let [config (find-by-id available plant (get current plant))]
    [:div
     [:h3 (str (i18n [:anomalies/reference-period "Referenzzeitraum"]) ": ")
      (case (count (get available plant))
        0 (i18n [:anomalies/no-config "keine Konfiguration"])
        1 (lbl-fn config)
        [bs/DropdownButton
         {:id "algselector"
          :title (or (lbl-fn config) "n/a")
          :onSelect (fn [idx]
                      (let [id (get-in available [plant idx :id])]
                        (dispatch [:anomalies/use-config plant id])))}
         (doall (for [[i cfg] (map-indexed vector (get available plant))]
                  ^{:key i} [bs/MenuItem {:id i
                                          :eventKey i}
                             (lbl-fn cfg)]))])]]))

(defn configuration-panel
"Configuration that determines which/how much details about anomalies will be shown"
  []
  (let [data (subscribe [:anomalies/view-data])
        state (subscribe [:anomalies/state])
        num (subscribe [:anomalies/minimum-indications])
        distinct-models (subscribe [:anomalies/model-date-ranges])
        date-range (subscribe [:anomalies/date-range])
        prevent-default #(.preventDefault %)]
    (fn []
      (let [{:keys [plant]} @data
            {:keys [all-candidates?] {:keys [current available]} :configs} @state]
        [:div
         [:div.row
          [:button.btn.btn-default.col-sm-3
           {:class (when all-candidates? "active")
            :disabled (= 1 @num)
            :title (i18n [:anomalies/all-results-tooltip "Zeige Anomaliekandidaten an Tagen, an denen die Heuristik keine eindeutigen Sensoren als Ursache erkennen konnte"])
            :on-click #(dispatch [:anomalies-page/toggle-candidates])} (i18n [:anomalies/all-results "Alle Ergebnisse"])]
          [:div.form-group.col-sm-3
           [:label (i18n [:anomalies/min-hints "Min. Hinweise"])]
           [:input.form-control {:type "number" :min 1 :step 1 :value @num :on-change #(dispatch [:anomalies/minimum-indications (js/parseInt (-> % .-target .-value))])}]]]
         [:div.form-group
          [:label (i18n [:anomalies/time-interval "Zeitbereich"])]
          [date-range-picker (state/cursor date-range [:anomalies/set-dates])]]
         [:div.form-group
          [:label (i18n [:anomalies/switch-all "Umschalten aller Anomaliemodelle"])]
          [:div.form-control
           [bs/DropdownButton {:id "uniquemodels"
                               :title ""
                               :onSelect (fn [idx] (dispatch [:anomalies/use-configs (nth @distinct-models idx)]))}
            (doall (for [[idx range] (map-indexed vector @distinct-models)]
                     ^{:key idx}
                     [bs/MenuItem {:id idx
                                   :eventKey idx}
                      (lbl-fn range)]))]]]
         (doall
          (for [[plant configs] (sort-by first available)
                :let [selected-id (get (or current {}) plant)
                      selected (find-by-id available plant selected-id)]]
            ^{:key plant}
            [:div.form-group
             [:label plant]
             [bs/DropdownButton {:id (str plant "configs")
                                 :title (if selected (lbl-fn selected) "n/a")
                                 :onSelect (fn [id] (dispatch [:anomalies/use-config plant id]))}
              (doall
               (for [{id :id :as cfg} configs]
                 ^{:key id}
                 [bs/MenuItem {:id (str "config" id)
                               :eventKey id}
                  (lbl-fn cfg)]))]]))]))))


(defn count-occurances [candidates sensor-id n]
  (let [models (filter #(str/starts-with? % sensor-id) (keys candidates))]
    {:|models| (if (zero? (count models))
                 0
                 (let [suffix (state/series-suffix (first models))]
                   (last (str/split suffix "/"))))
     :|divergent| (count models)
     :|predictors| (- n (count models))}))

(defn thresholds-table
  "Render results of threshold violations."
  []
  [dt/datatable ::thresholds [:anomalies/threshold-violations]
   [{::dt/column-key [:timestamp] ::dt/column-label (i18n [:date "Datum"])   ::dt/render-fn (fn [ts] [:span (f/human-date ts)])}
    {::dt/column-key [:plant]     ::dt/column-label (i18n [:plant "Anlage"]) ::dt/render-fn plant-button}
    {::dt/column-key [:since]     ::dt/column-label (i18n [:since "Seit"])   ::dt/render-fn (fn [since {ts :timestamp}] [duration-in-days since ts])}
    {::dt/column-key [:type]      ::dt/column-label (i18n [:anomalies/change-type "Meldung"]) ::dt/render-fn (fn [k] [:span (str k)])}
    {::dt/column-key [:sensor]    ::dt/column-label (i18n [:sensor "Sensor"]) ::dt/render-fn (fn [s-name {ts :timestamp p :plant}] [sensor s-name (time/minus ts (time/days 1)) ts p])}
    {::dt/column-key [:duration]  ::dt/column-label (i18n [:anomalies/duration-in-day "Dauer pro Tag"])}]
   {::dt/table-classes ["table" "table-compact" "table-striped"]}])

(defn lasso-results
"Render lasso regression model results."
  []
  (let [data (subscribe [:anomalies/view-data])
        state (subscribe [:anomalies/state])
        num (subscribe [:anomalies/minimum-indications])]
    (fn []
      (let [{:keys [plant anomalies details-for]} @data
             {:keys [configs]} @state
            single-plant? (boolean plant)]
        [:div
         (when single-plant?
           [algorithm-selector configs plant])
         [:div.input-group.pull-right
            [bs/popup {:placement "left"}
             (i18n [:anomalies/settings "Einstellungen"])
             [configuration-panel]]]
         [:table {:class "table"}
          [:colgroup
           [:col {:style {:width "15%"}}]
           (when (not single-plant?) [:col {:style {:width "10%"}}])
           [:col {:style {:width "15%"}}]
           [:col {:style {:width "40%"}}]
           [:col {:style {:width "30%"}}]]
          [:thead
           [:tr
            [:th (i18n [:date "Datum"])]
            (when (not single-plant?) [:th (i18n [:plant "Anlage"])])
            [:th (i18n [:since "Seit"])]
            [:th (i18n [:sensor "Sensor"])]
            [:th (i18n [:anomalies/number-hints "Anzahl Hinweise"])]]]
          (reduce into [:tbody]
                  (for [{timestamp :timestamp, plant :plant, {:keys [faults since candidates]} :results, alg :algorithm} anomalies
                        :let [anomalous? (fn [s] (some #(when s (str/starts-with? s %)) (keys faults)))
                              {plant-details :plant ts-details :timestamp} details-for
                              show-models? (and (= plant plant-details) (= timestamp ts-details))
                              first-col [:td {:rowSpan (+ (count faults) (if show-models? 1 0))}
                                         [:div.container-fluid ;{:style {:min-width 200}}
                                          [:div.row (f/human-date timestamp)
                                           [:button.btn.btn-default {:class (when show-models? "active")
                                                                     :title (i18n [:anomalies/models-tooltp "Detailansicht der auffälligen Regressionsmodelle"])
                                                                     :on-click #(dispatch [:anomalies/toggle :details-for {:plant plant :timestamp timestamp}])} (i18n [:anomalies/models "Modelle"])]]]]]]
                    (cons ;; if we toggled the button, insert detail table
                     (when show-models?
                       [:tr first-col
                        [:td {:col-span 6}
                         [bs/Panel {:collapsible true
                                    :expanded show-models?
                                    :header (str (i18n [:anomalies/details-title "Details der Anomalieerkennung"]) " " (f/human-date timestamp))}
                          [models-table candidates timestamp plant anomalous?]]]])
                     (for [[idx [sensor-id n]] (map-indexed vector (reverse (sort-by (comp :n second) faults)))
                           :let [{:keys [|models| |divergent| |predictors|]} (count-occurances candidates sensor-id n)]] 
                       ;; default table rows
                       ^{:key (str/join "_" [alg plant idx timestamp sensor-id])}
                       [:tr 
                        (when (and (not show-models?) (zero? idx))
                          first-col)
                        ;; button to switch to individual plant
                        (when (not single-plant?) [:td [plant-button plant]])
                        [:td [duration-in-days (since sensor-id) timestamp]]
                        [:td [sensor sensor-id (since sensor-id) timestamp plant]]
                        [:td (i18n [:anomalies/occurances [:span "**%1** (%2/%3 Modelle, %4 Prädiktoren)"]] [n |divergent| |models| |predictors|])]]))))]]))))

(defn anomalies-page []
  (let [data (subscribe [:anomalies/view-data])
        plants (subscribe [:all-plant-names])
        tab-name (subscribe [:anomalies/active-tab])
        cb #(dispatch [:anomalies/active-tab %])]
    ;; remember scroll position on umount
    (pc/scroll-position-aware :anomalies-page
     (fn []
       (let [{:keys [plant]} @data]
         [pc/page
          [:div
           [:h2 (i18n [:anomalies/anomalies "Anomalien"]) (when plant (str " in " plant))]
           [plant-selector plants plant]]          
          [bs/Tabs {:active-key @tab-name :on-select cb}
           [bs/Tab {:title "Schwellwerte" :event-key "thresholds"}
            [thresholds-table]]
           [bs/Tab {:title "Anomalien" :event-key "lasso"}            
            [lasso-results]]]])))))
