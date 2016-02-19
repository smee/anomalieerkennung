(ns ^:figwheel-no-load gt1-ui.dev
  (:require [gt1-ui.core :as core]
            [figwheel.client :as figwheel :include-macros true]
            [devtools.core :as devtools]))

(enable-console-print!)
; see https://github.com/binaryage/cljs-devtools
(devtools/set-pref! :install-sanity-hints true) ; for better stacktraces in case of dereferencing nulls
(devtools/install!)

(figwheel/watch-and-reload
  :websocket-url "ws://localhost:3449/figwheel-ws"
  :jsload-callback core/mount-root)

(core/init!)
