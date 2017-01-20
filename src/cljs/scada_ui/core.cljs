(ns scada-ui.core
  (:require [goog.object :as obj]
            [accountant.core :as accountant]
            [alandipert.storage-atom :as asa :refer [local-storage]]
            [scada-ui.util.transit :as tr]
            cljsjs.d3
            ;;cljsjs.vis
            [cljs.reader :refer [read-string]]
            [cljs-time.coerce :as tc]
            [cljs-time.core :as time]
            cljs-time.extend
            [cljs-time.format :as tf]          
            [scada-ui.components.table :as table]
            [reagent.core :as reagent]
            [reagent.interop :refer [$]]
            [re-frame.core :as re :refer [reg-cofx reg-fx inject-cofx reg-event-db reg-event-fx path reg-sub dispatch dispatch-sync subscribe]]
            [secretary.core :as secretary]
            [scada-ui.state :as state]
            [scada-ui.format :as f]
            [scada-ui.pages.common :as pc]
            scada-ui.pages.anomalies-page
            scada-ui.pages.modes-page
            scada-ui.pages.wind-page
            scada-ui.pages.charts-page
            scada-ui.pages.sensors-page
            [scada-ui.util.tree :as tree]
            [scada-ui.i18n :refer [i18n]]
            [scada-ui.remote :as remote]))

(declare local-settings structure-versions)

;;---------------------------  sticky settings ---------------------------------------------------------------
;; keep all switches across browser reloads
;;
(def sticky-paths "Some parts of the application model should be kept persistent between individual sessions (e.g. localstorage). Used mainly for configuration settings."
  [[:chart :flags]
   [:chart :visibility]
   [:modes-page :color-class]
   [:wind-page :sector-angle]
   [:anomalies-page :candidates?]
   [:anomalies-page :|n|]
   [:anomalies-page :configs :current]
   [:language]])

(defn copy-deep
  ([m paths] (copy-deep m paths {}))
  ([m paths res] (reduce (fn [res path] (let [v (get-in m path)]
                                         (if v (assoc-in res path v) res))) res paths)))


(defn initialize-persistent-settings
"Add persisted state from local storage to default application state."
  [dflt-state]
  (defonce settings-watcher
    (add-watch re-frame.db/app-db ::sticky-settings
       (fn [_ _ old new]
         (let [old' (copy-deep old sticky-paths)
               new' (copy-deep new sticky-paths)]
           (when (not= old' new')
             (reset! local-settings new'))))))
  (copy-deep @local-settings sticky-paths dflt-state))

;; read initial application state
(reg-event-fx :core/initialize ;; step #1
   (fn [{:keys [db]} [_ demo?]]
     (let [langs (mapv keyword (or (.-languages js/navigator) ["de" "en"]))
           dflt-state {:plants []
                       :structure {:table []}
                       :show-plants? false
                       :show-calendar? false
                       :chart scada-ui.pages.charts-page/default-state
                       :tree {:selected? #{}
                              :active nil
                              :expanded? #{}
                              :filter {:show? false
                                       :text ""}}
                       :languages langs
                       :language (first langs)
                       :modes-page scada-ui.pages.modes-page/default-state
                       :anomalies-page scada-ui.pages.anomalies-page/default-state
                       :wind-page scada-ui.pages.wind-page/default-state
                       :sensors-page scada-ui.pages.sensors-page/default-state
                       :power-curve {:configurations {}}}
           new-db (initialize-persistent-settings dflt-state)
           plants (read-string (.-textContent (.getElementById js/document "plants")))]       
       {:remote (when (empty? plants) {:url "/db/plants" :success [:core/plants-loaded demo?]})
        :dispatch [:core/plants-loaded demo? plants]
        :db new-db})))

(defn shuffle-coordinates [plants]
  (let [circles (js/d3.packSiblings (clj->js (map #(assoc % :r 1) plants)))
        by-name (group-by :name plants)]
    (reduce (fn [res js-obj]
              (let [n (obj/get js-obj "name")
                    x (obj/get js-obj "x")
                    y (obj/get js-obj "y")]
                (if n
                  (conj res (assoc (first (by-name n)) :latitude x :longitude y))
                  res))) [] circles)))

(reg-event-fx :core/plants-loaded ;; step #4
   (fn [{db :db} [_ demo? plants]]
     (let [plants (if demo? (shuffle-coordinates plants) plants)
           backend-versions (read-string (.-textContent (.getElementById js/document "versions")))
           types (distinct (map :type plants))
           names-to-fetch (filter #(not= (:hash (get @structure-versions %)) (get backend-versions %)) types)]
       {:db (assoc db :plants plants)
        :dispatch-n [[:create-default-powercurve-configs]
                     [:core/structure-loaded (vals @structure-versions)]]
        :remote (when (not-empty names-to-fetch)
                  {:url "/db/structures"
                   :params {:names names-to-fetch}
                   :success [:core/structure-loaded]})})))


(reg-event-db :core/structure-loaded ;; step #5
   (fn [db [_ [structure :as structures]]] ;; TODO handle multiple plant structures
     (swap! structure-versions (fn [m] (reduce #(assoc %1 (:name %2) %2) m structures)))
     (accountant/dispatch-current!) ;; we now have all the data we need, time to navigate to the user-selected page
     (assoc db :structure structure)))

;; -----------------------------------------------------------------------------------
;; Routes and in-app navigation

(reg-event-db :current-page [(path :current-page)]
              (fn [db [_ page suffix]]
                (-> db
                    (assoc :current page)
                    (assoc-in [:pages page] suffix))))

(state/reg-sub :current-page [:current-page])
(reg-sub :current-link :<- [:current-page]
         (fn [{:keys [current pages]}]
           (pc/link-of current (get pages current))))

(def pages {:sensors-page scada-ui.pages.sensors-page/sensors-page
            :anomalies-page scada-ui.pages.anomalies-page/anomalies-page
            :statistics-page scada-ui.pages.modes-page/mode-statistics-page
            :home-page scada-ui.pages.charts-page/home-page
            :wind-page scada-ui.pages.wind-page/wind-directions})

(secretary/defroute "/charts" []
  (dispatch [:current-page :home-page]))

(secretary/defroute "/sensors" []
  (dispatch [:sensors-page/initialize])
  (dispatch [:current-page :sensors-page]))

(secretary/defroute "/" []
  (dispatch [:modes/initialize])
  ;; use yesterday
  (dispatch [:modes/change-date (time/minus state/today (time/days 1))])
  (dispatch [:current-page :statistics-page]))

(secretary/defroute #"/(\d\d\.\d\d.\d\d\d\d)" [date]
  (dispatch [:modes/initialize])
  (dispatch [:modes/change-date (f/parse-human-date date)])
  (dispatch [:current-page :statistics-page date]))

(secretary/defroute "/anomalies" []
  (dispatch [:anomalies/initialize])
  (dispatch [:current-page :anomalies-page]))

(secretary/defroute "/anomalies/:plant" [plant]
  (dispatch [:anomalies/initialize plant])
  (dispatch [:current-page :anomalies-page plant]))

(secretary/defroute "/wind" []
  (dispatch [:current-page :wind-page]))

(defn current-page []
  (let [{page :current} @(subscribe [:current-page])]
    (if-not page
      [:span (i18n [:please-wait])]
      [(pages page)])))

(defn navigate-to-uri [uri] (secretary/dispatch! uri))
;; register new effect :navigate, takes a uri as string parameter
(reg-fx :navigate accountant/navigate!)
;; register global dispatch handler :navigate, for on-clicks etc.
(reg-event-fx :navigate (fn [cofx [_ uri]] {:navigate uri}))

;; -------------------------
;; Initialize app

#_(defn timeline []
  (reagent/create-class
     {:component-will-update
      (fn [this]
        (let [chart (:chart (reagent/state this))]))
      :component-will-unmount (fn [this] ;;remove all listeners, give dygraphs a chance to clear up memory
                                ($ (:chart (reagent/state this)) destroy))
      :component-did-mount
      (fn [this]
                (let [dom (reagent/dom-node this)
                      element (aget (.getElementsByClassName dom "timeline") 0)
                      items (js/vis.DataSet. (clj->js [{:id 1 :content "item 1" :start "2016-04-01" :end "2016-04-03"}
                                                       {:id 2 :content "item 2" :start "2016-04-02" :end "2016-04-03"}]))
                      timeline (js/vis.Timeline. element items #js {:locale "de"})]
                  (reagent/replace-state this {:chart timeline})))
      :reagent-render (fn []
                        [:div
                         [:div.timeline "Chart goes here"]])}))

(defn mount-root []
  (reagent/render #_[timeline] [current-page] (.getElementById js/document "app")))


(defn init! []
  ;; allow us to use HTMLCollection etc. as seqs
  (extend-type js/NodeList
    ISeqable
    (-seq [array] (array-seq array 0)))
  (extend-type js/HTMLCollection
    ISeqable
    (-seq [array] (array-seq array 0)))
  
  ;; make sure that transit based local storage persistence knows how to handler our custom data types
  (reset! asa/transit-read-handlers tr/readers)
  (reset! asa/transit-write-handlers tr/writers)  
  ;; make sure clicking on links to other in-app links does not trigger a page reload
  (accountant/configure-navigation! {:nav-handler navigate-to-uri :path-exists? secretary/locate-route})  
  (pc/run-scroll-listener!)
  ;; cached application settings 
  (defonce local-settings (asa/local-storage (atom {}) ::sticky-settings))
  ;; cached plant structure definitions (quite big)
  (defonce structure-versions (asa/local-storage (atom {}) ::structure-versions))
  (dispatch-sync [:core/initialize (= "true" (.-textContent (.getElementById js/document "demo")))])
  (mount-root))
