(ns gt1-ui.prod
  (:require [gt1-ui.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
