(ns ^:figwheel-no-load scada-ui.dev
  (:require [scada-ui.core :as core]
            [figwheel.client :as figwheel :include-macros true]
            [devtools.core :as devtools]
            dirac.runtime
            [re-frame.subs :as re]))

(enable-console-print!)
; see https://github.com/binaryage/cljs-devtools
; for better stacktraces in case of dereferencing nulls
(devtools/install! [:custom-formatters :sanity-hints])
(dirac.runtime/install!)

(figwheel/watch-and-reload
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :jsload-callback (fn []
                     (re/clear-subscription-cache!) ;; FIXME only reactivate when we don't rely on subscriptions outside of rendering functions anymore
                     (core/mount-root)))

(core/init!)
