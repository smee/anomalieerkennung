(ns scada-ui.pages.common
  (:require [reagent.core :as reagent]            
            [re-frame.core :as re :refer [reg-sub reg-event-db reg-event-fx path dispatch subscribe reg-fx]]
            [scada-ui.components.bootstrap :as bs]            
            [scada-ui.i18n :refer [i18n]]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.events.EventType :as EventType]
            [goog.Timer :as timer]))


;; ----------------- progress bar when loading asynchronously ------------------------------------------------------
(def in-progress? (reagent/atom #{}))
(defn loading-bar []
  (let [active? (boolean (not-empty @in-progress?))]
    [bs/ProgressBar {:active active?
                     :now (if active? 100 0)
                     :striped true
                     :label (i18n [:loading])}]))

(defn color-cube [color {:keys [size] :or {size 20}}]
  [:div {:style {:width size
                 :margin-right 10
                 :display "inline-block"
                 :height size                     
                 :background-color color}}])

;; --------------------- common toolbar ---------------------------------------------------------------------------
(def urls [{:url "/" :key :statistics-page :label :titles/availability}
           {:url "/charts" :key :home-page :label :titles/scadadata}
           {:url "/anomalies" :key :anomalies-page :label :titles/anomalies}
           {:url "/sensors" :key :sensors-page :label :titles/sensors} 
           {:url "/wind" :key :wind-page :label :titles/wind}])

(defn link-of [page-key suffix]
  (let [{url :url :as res} (some #(when (= (:key %) page-key) %) urls)]
    (str url (when suffix (str "/" suffix)))))

(defn toolbar []
  (let [{:keys [current pages]} @(subscribe [:current-page])]
    [:ul {:class "nav nav-tabs"}
     (doall
      (for [{:keys [url key label]} urls
            :let [link (link-of key (get pages key))]]
        ^{:key key}
        [:li {:class (when (= current key) "active")}
         [:a {:href link} (i18n [label])]]))
     [:li.pull-right
      [bs/DropdownButton {:title (i18n [:language "Sprache"])
                          :id "langselector"
                          :onSelect (fn [lang] (dispatch [:i18n/select-language (keyword lang)]))}
       (doall (map-indexed (fn [idx lang] ^{:key idx}
                             [bs/MenuItem {:id idx
                                           :href ""
                                           :eventKey lang} lang]) (keys scada-ui.i18n/dict)))]]]))

(defn header []
  [:header
   [toolbar]
   [loading-bar]])

(defn page [& children]
  (into [:div [header]] children))

;; ---------------- debounce ------------------------------------------------------------------------------------------------

(defn debounce-factory
  "Return a function that will always store a future call into the
  same atom. If recalled before the time is elapsed, the call is
  replaced without being executed." []
  (let [f (atom nil)]
    (fn [func ttime]
      (when @f
        (timer/clear @f))
      (reset! f (timer/callOnce func ttime)))))

;;;;;; scrolling position info ;;;;;;;;;;;;;;;;;;;;;
(defonce cur-scroll-y (atom 0))

(defn run-scroll-listener!
"constantly store the most recent y scroll position of the page in `cur-scroll-y`."
  []
  (let [debounce (debounce-factory)]
    (events/listen js/window EventType/SCROLL (fn [] (debounce #(reset! cur-scroll-y (.-y (dom/getDocumentScroll))) 250)))))

(reg-sub :scroll-position (fn [db [_ page-name]] (get-in db [:scroll-positions page-name] 0)))
(reg-event-db :scroll-position (fn [db [_ page-name position]] (assoc-in db [:scroll-positions page-name] position)))

(defn scroll-position-aware
"Wrapper for pages that want to store/restore their vertical scroll position on un-/mounting."
  [page-name page]
  (let [scroll-position (subscribe [:scroll-position page-name])]
    (reagent/create-class
     {:component-did-mount (fn [] (js/window.scrollTo 0 @scroll-position))
      :component-will-unmount #(dispatch [:scroll-position page-name @cur-scroll-y])
      :reagent-render (fn [] [page])})))

(defonce window-width
  (let [atm (reagent/atom {:width 0
                           :height 0
                           :_touched 0})]
    ;; keep artificial :_touched value to make sure, listeners get notified about each RESIZE event,
    ;; even if the real dimensions do not change
    (events/listen js/window EventType/RESIZE
                   #(swap! atm (fn [{t :_touched}] {:_touched (inc t)
                                                  :width (.-innerWidth js/window)
                                                  :height (.-innerHeight js/window)})))
    atm))
