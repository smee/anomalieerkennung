(ns gt1-ui.core
  (:require [accountant.core :as accountant]
            [alandipert.storage-atom :refer [local-storage]]
            cljsjs.d3
            [cljs.reader :refer [read-string]]
            [cljs-time.coerce :as tc]
            [cljs-time.core :as time]
            cljs-time.extend
            [cljs-time.format :as tf]          
            [gt1-ui.table :as table]            
            [historian.core :as hist :refer-macros [off-the-record]]
            historian.keys
            [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [gt1-ui.pages.common :as pc]
            gt1-ui.pages.anomalies-page
            gt1-ui.pages.modes-page
            gt1-ui.pages.wind-page
            gt1-ui.pages.charts-page))


(defn series-table []
  [:div
   [pc/toolbar]
   [:div {:style {:background-color "white"}}
    [:h2 "Sensoren"]
    (if-let [data (session/get-in [:structure :table])]      
      [table/table data {:hideable? true}]
      [:p "Bitte warten...."])]])

(defn current-page []
  (apply vector
         (get {:series-table series-table
               :anomalies-page gt1-ui.pages.anomalies-page/anomalies-page
               :statistics-page gt1-ui.pages.modes-page/mode-statistics-page
               :home-page gt1-ui.pages.charts-page/home-page
               :wind-page gt1-ui.pages.wind-page/wind-directions}
              (session/get :current-page :home-page))
         (session/get :current-args [])))


;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page :home-page))

(secretary/defroute "/table" []
  (session/put! :current-page :series-table))

(secretary/defroute "/availability" []
  (accountant/navigate! (str "/availability/" (tf/unparse pc/human-df (time/minus (time/today-at-midnight) (time/days 1))))))

(secretary/defroute #"/availability/(\d\d\.\d\d.\d\d\d\d)" [date]
  (hist/with-single-record
    (session/put! :current-page :statistics-page)
    (session/put! :current-args [(tf/parse pc/human-df date)])))

(secretary/defroute "/anomalies" []
  (session/put! :current-page :anomalies-page))

(secretary/defroute "/anomalies/:plant" [plant]
  (hist/with-single-record
    (session/put! :current-page :anomalies-page)
    (session/put! :current-args [plant])))

(secretary/defroute "/wind" []
  (session/put! :current-page :wind-page))
;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn navigate-to-uri [uri]
  (secretary/dispatch! uri))

(defn init! []
  ;; read CSRF token to use for POST requests
  (reset! pc/security-token (.-textContent (.getElementById js/document "security-token")))
  ;; read initial application state
  (session/reset! (read-string (.-textContent (.getElementById js/document "initial-state"))))
  ;; make sure clicking on links to other in-app links does not trigger a page reload
  (accountant/configure-navigation! {:nav-handler navigate-to-uri :path-exists? secretary/locate-route})
  (accountant/dispatch-current!)
  (hist/record! session/state :global-state)
  (hist/record! gt1-ui.pages.modes-page/stats :availability-stats)
  (historian.keys/bind-keys)
  (pc/run-scroll-listener!)
  (mount-root))
