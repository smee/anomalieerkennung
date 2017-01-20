(ns scada-ui.format
  (:require [clojure.string :as str]
            [cljs-time.core :as time]
            [reagent.format :as format]
            [cljs-time.format :as tf]
            goog.math))

(def human-df (tf/formatter "dd.MM.yyyy"))

(def format format/format)

(defn human-date [date]
  (tf/unparse human-df date))

(defn parse-human-date [s]
  (tf/parse human-df s))

(defn date-only [dt]
  (time/date-time (time/year dt) (time/month dt) (time/day dt)))

(defn date-time->str
"For backend communications, use for parameters in requests."
  [dt]
  (tf/unparse (tf/formatters :date-time-no-ms) dt))

(defn date->str
"For backend communications, use for parameters in requests."
  [dt]
  (tf/unparse (tf/formatters :date) dt))

(defn format-number
"Format a number so that it shows max. `digits` significant, non-zero digits after the comma.
Idea: use .toPrecision on the part after the comma only."
  ([num] (format-number num 4))
  ([num digits]
   (if (or (not (number? num)) (js/isNaN num))
     (str num)
     (let [int (if (neg? num) (js/Math.ceil num) (js/Math.floor num))
           num' (js/Math.abs (- num int))
           c (->> (.toPrecision num' digits)
                  seq
                  (drop 2)
                  reverse
                  (drop-while #(= "0" %))
                  reverse
                  (cons ".")
                  (apply str))
           c (if (str/index-of c "e-") "." c)] ;; if num' is very small, c will be something like '441e-16'
       (str (when (neg? num) "-") int (when (not= "." c) c))))))
