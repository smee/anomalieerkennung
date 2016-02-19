(ns gt1-ui.defaults
  (:require [#?(:cljs cljs-time.core
                :clj clj-time.core) :as time]))

(def num-data-points 200)

(def minimum-time-interval "minimum distance between data points in seconds" (* 10 60))

(def status-codes {0 {:name "sonstige" :color "black"}
                   1 {:name "Fehler" :color "#FD0100"}
                   2 {:name "Netzausfall" :color "#890302"}
                   3 {:name "Initialisierung" :color "#F9F0EB"}
                   4 {:name "Bereit" :color "#93EE91"}
                   5 {:name "Hochlauf" :color "#FB02FC"}
                   6 {:name "Netzersatzbetrieb UW" :color "#FFA37C"}
                   7 {:name "Produktion" :color "#008300"}
                   8 {:name "Schattenwurf" :color "#68B1FF"}
                   9 {:name "Eisansatz" :color "#7F8180"}
                   10 {:name "Setup" :color "#7F017F"}
                   11 {:name "Systemtest" :color "#5F95EB"}
                   12 {:name "Windabschaltung" :color "#83FCD9"}
                   13 {:name "Service" :color "#FCFE04"}
                   14 {:name "Manueller Stop" :color "#FFA200"}
                   15 {:name "Netzersatzbetrieb WEA" :color "#FA8274"}
                   16 {:name "Inselbetrieb" :color "black"}})

(def in-production #{4 5 7})

(def color-classes ["YlGn", "YlGnBu", "GnBu", "BuGn", "PuBuGn", "PuBu", "BuPu", "RdPu", "PuRd", "OrRd", "YlOrRd", "YlOrBr", "Purples", "Blues", "Greens", "Oranges", "Reds", "Greys", "PuOr", "BrBG", "PRGn", "PiYG", "RdBu", "RdGy", "RdYlBu", "Spectral", "RdYlGn"])
