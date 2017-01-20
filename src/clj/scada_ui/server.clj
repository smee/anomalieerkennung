(ns scada-ui.server
  (:require [scada-ui.handler :refer [app]]
            [environ.core :refer [env]]
            [org.httpkit.server :refer [run-server]])
  (:gen-class))

 (defn -main [& args]
   (let [port (Integer/parseInt (or (env :port) "3000"))]
     (run-server app {:port port
                      :join? false
                      :max-line 10000})))
