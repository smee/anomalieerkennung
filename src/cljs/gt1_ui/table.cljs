(ns gt1-ui.table
  (:require [reagent-table.core :as rt]
            [cljs-time.core :as time :refer [date-time]]
            [cljs-time.format :as tf]
            [gt1-ui.defaults :refer [status-codes]]))

(defn table [data-or-atom {:keys [caption class hideable?]
                           :or {class "table table-hover table-striped table-bordered"}}]
  (let [opts {:table {:class class}
              :caption caption
              }]
    [rt/reagent-table data-or-atom
     (if hideable?
       (assoc opts :rows-selection {:ul
                                    {:li {:class "btn btn-default"}}})
       opts)]))

(defn error-table [errors]
  [:table {:class "table table-compact table-striped"}
   [:thead
    [:tr (for [lbl ["Anlage" "Start" "Dauer" "Komponente" "Fehler" "Fehler-ID"]]
           ^{:key lbl} [:th lbl])]]
   [:tbody
    (for [[plant vs] errors,
          {s :timestamp, e :end c :component error :error} vs
          :let [diff (time/interval s e)]]
      ^{:key (str plant s e)}
      [:tr
       [:td plant]
       [:td (tf/unparse (tf/formatter "dd.MM.yyyy HH:mm:ss") s)]
       [:td (tf/unparse-duration diff)]
       [:td (:label c)]
       [:td (:label error)]
       [:td (:id error)]])]])

(def fmt (tf/formatter "dd.MM.yyyy HH:mm:ss"))

(defn modes-table [plant modes]
  [:table {:class "table table-compact table-striped"}
   [:thead
    [:tr
     [:th "Anlage"] [:th "Start"] [:th "Dauer"] [:th "Zustand"]]]
   [:tbody
    (for [{:keys [timestamp end mode]} modes
          :let [diff (time/interval timestamp end)
                {label :name color :color} (get-in status-codes [mode])]]
      ^{:key (str timestamp label)}
      [:tr
       [:td plant]
       [:td (tf/unparse fmt timestamp)]
       [:td {:title (tf/unparse fmt end)} (tf/unparse-duration diff)]
       [:td [:div {:style {:display "inline-block" :width 10 :height 10 :background-color color :margin-right 10}}] label]])]])


