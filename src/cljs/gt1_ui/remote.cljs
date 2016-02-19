(ns gt1-ui.remote
  (:require [ajax.core :include-macros true]
            [gt1-ui.util.transit :as tr]
            [gt1-ui.pages.common :as pc]
            [cognitect.transit :as ct]
            [reagent.core :as reagent]))



(defn GET
"Same as ajax.core/GET but inserts options needed to transparently read JODA date-time instances.
Interceptors are not sufficient, because changes to the response format are not allowed?"
  [url opts]
  (let [orig-handler (:handler opts)]
    (reset! pc/in-progress? true)
    (ajax.core/GET url (assoc opts
                              :handler (fn [resp] 
                                         (try (orig-handler resp)
                                              (finally (reset! pc/in-progress? false))))
                              :response-format (ajax.core/transit-response-format
                                                {:reader (ct/reader :json {:handlers tr/readers})})))))

(defn POST [url opts]
  (let [orig-handler (:handler opts)]
    (ajax.core/POST url (assoc-in opts [:headers "X-XSRF-TOKEN"] @pc/security-token))))
