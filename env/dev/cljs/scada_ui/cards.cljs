(ns scada-ui.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.interop :refer-macros [$]]
            reagent.ratom
            [reagent.session :as session]
            [scada-ui.core :as core]
            [scada-ui.components.calendar :as cal]
            [scada-ui.components.tree :as tree]
            [scada-ui.components.table :as table]
            [scada-ui.components.chart :as chart]
            [scada-ui.components.park :as park]
            [scada-ui.components.bootstrap :as bs]
            [scada-ui.components.heatmap :as hm]
            ajax.core
            scada-ui.cards.park
            scada-ui.cards.calendar
            scada-ui.cards.matrix
            [devtools.core :as devtools]
            [scada-ui.format :as f]
            [cljs-time.core :as time])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest doc]]))

(defonce initial-setup
  (do
    (enable-console-print!)
    (devtools/set-pref! :install-sanity-hints true) ; for better stacktraces in case of dereferencing nulls
    (devtools/install!)))


(let [atm (atom [])]
  (ajax.core/GET "/db/heatmap" {:params {:series ["2/14/12/avg" "1/2/7/avg"]
                                         :x-steps 80
                                         :y-steps 100
                                         :plants ["GT79"]
                                         :dates [(f/date-time->str (time/date-time 2015)) (f/date-time->str (time/date-time 2017))]}
                             :handler (fn [resp] (reset! atm resp))})
  (defcard-rg heatmap-card
    (fn [atm]
      (let [w 200
            h 200]
        [:div.container-fluid
         (doall
          (for [[header colors] [["Inferno" hm/inferno] ["Viridis" hm/viridis]]]
            ^{:key header}
            [:div.row
             [:h4 header]
             [:div.col-md-3
              [:h5 "Standard"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? false :inverse? false}]]
             [:div.col-md-3
              [:h5 "Grey"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? true :inverse? false}]]
             [:div.col-md-3
              [:h5 "Inverse"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? false :inverse? true}]]
             [:div.col-md-3
              [:h5 "Inverse grey"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? true :inverse? true}]]
             [:div.col-md-3
              [:h5 "Standard logarithmic"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? false :inverse? false :log? true :axis? false}]]
             [:div.col-md-3
              [:h5 "Grey logarithmic"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? true :inverse? false :log? true  :axis? false}]]
             [:div.col-md-3
              [:h5 "Inverse logarithmic"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? false :inverse? true :log? true :axis? true :ticks 2}]]
             [:div.col-md-3
              [:h5 "Inverse grey logarithmic"]
              [hm/heatmap (:data @atm) (:extents @atm) {:width w :height h :colors colors :grey? true :inverse? true :log? true :axis? true :ticks 2}]]]))]))
    atm))

(defcard-rg wind-rose
  (let [sectors (range 0 360 30)
        values (repeatedly (count sectors) #(hash-map :n (rand) :v (rand-int 20)))
        sum (apply + (map :n values))
        values (zipmap sectors (map #(update % :n / sum) values))]
    [:div
     [park/wind-rose 50 "foo" values]
     [park/wind-rose 100 "foo" values]
     [park/wind-rose 200 "bar" values]
     [park/wind-rose 400 "baz" values]]))


(defcard-rg pagination
  [bs/Pagination {:bs-size "sm"
                  :items 12 ;; aka pages
                  :first true
                  :prev true
                  :next true
                  :last true
                  :max-buttons 7
                  :active-page 9
                  :on-select #()}])

(defcard dates
  (let [d1 #inst "2013-01-02"
        d2 #inst "2013-01-02"
        m {d1 true}]
    [d1 d2 (get m d1) (get m d2)]))


(let [state (atom [[1,[0 1 2]]
                   [2,[1 2 3]]
                   [3,[2 3 4]]
                   [4,[.5 2 2.2]]
                   [5 [6 7 10]]])]
  #_(js/setInterval #(swap! state conj [7 [ 1 1 1]]) 2000)

  (defcard-rg dygraph
    (fn [data _]
      [:div {:width 700}
       [chart/line-chart @data {:labels ["Daten"]}]])
    state))


(defcard-rg menu-bar
  [:nav.navbar.navbar-default
   [:ul.nav.navbar-nav
    [:li [:a "test"]]
    [:li.dropdown
     [:a.dropdown-toggle {:data-toggle "dropdown"} "Dropdown" [:span.caret]]
     [:ul.dropdown-menu
      [:li>a "foo"]
      [:li>a "bar"]
      [:li.divider {:role "separator"}]
      [:li>a "baz"]]]]])

#_(defcard-rg tree
  (fn [tree-atom _]
    [tree/tree tree-atom (atom {}) {:callback println}])
  (atom [{:label "root"
          :expanded? true
          :children [{:label "c1"
                      :expanded? true}
                     {:label "c2"
                      :children [{:label "cc1"}]}
                     {:label "custom"
                      :icon "glyphicon-trash"}]}])
  {:inspect-data true})



; remove static content visible without javascript
(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards

