(ns gt1-ui.pages.common
  (:require [cljs-time.core :as time]
            [cljs-time.coerce :as tc]
            [cljs-time.format :as tf]
            [reagent.core :as reagent]
            [reagent.session :as session]
            [reagent.ratom :refer-macros [reaction]]
            [gt1-ui.bootstrap :as bs]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.events.EventType :as EventType]
            [goog.Timer :as timer]))

(def human-df (tf/formatter "dd.MM.yyyy"))

(defn human-date [date]
  (tf/unparse human-df date))

(defn parse-human-date [s]
  (tf/parse human-df s))

(defn date-time->str
"For backend communications, use for parameters in requests."
  [dt]
  (tf/unparse (tf/formatters :date-time-no-ms) dt))

;; CSRF prevention token for POSTS
(def security-token (atom nil))
;; plant coordinates
(def coords (reaction (mapv (juxt :latitude :longitude) (session/get :plants))))

(defn at
"Create cursor to a part of the global application state."
  [& ks]
  (reagent/cursor session/state (vec ks)))

(defn toggle!
  "Toggle a boolean value in global application state."
  [ks]
  (session/update-in! ks (comp not boolean)))
;;-------------------------
;; components

(def in-progress? (reagent/atom false))
(defn loading-bar []
  (let [active? @in-progress?]
    [bs/ProgressBar {:active active?
                     :now (if active? 100 0)
                     :striped true
                     :label "Lade..."}]))

(defn color-cube [color {:keys [size] :or {size 20}}]
  [:div {:style {:width size
                 :margin-right 10
                 :display "inline-block"
                 :height size                     
                 :background-color color}}])

(def urls [{:url "/" :key :home-page :label "Betriebsdaten"}
           {:url "/anomalies" :key :anomalies-page :label "Anomalien"}
           {:url "/table" :key :series-table :label "Sensoren"}
           {:url "/availability" :key :statistics-page :label "Verfügbarkeit"}])

(defn toolbar [additional]
  (let [active (session/get :current-page)] 
    [:nav#nav
     [:ul
      (for [{:keys [url key label]} urls]
        ^{:key key}
        [:li {:class (when (= active key) "active")}
         [:a {:href url} label]])      
      [:li.flex]
      (when additional
        additional)]]))

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
(def cur-scroll-y (atom 0))

(defn run-scroll-listener!
"constantly store the most recent y scroll position of the page in `cur-scroll-y`."
  []
  (let [debounce (debounce-factory)]
    (events/listen js/window EventType/SCROLL (fn [] (debounce #(reset! cur-scroll-y (.-y (dom/getDocumentScroll))) 250)))))
