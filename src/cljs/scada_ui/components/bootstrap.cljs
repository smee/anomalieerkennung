(ns scada-ui.components.bootstrap
  (:require [reagent.core :as reagent]
            [goog.object :as obj]
            cljsjs.react-bootstrap))

(def Button (reagent/adapt-react-class (obj/get js/ReactBootstrap "Button")))

(def ProgressBar (reagent/adapt-react-class (obj/get js/ReactBootstrap "ProgressBar")))

(def DropdownButton (reagent/adapt-react-class (obj/get js/ReactBootstrap "DropdownButton")))

(def SplitButton (reagent/adapt-react-class (obj/get js/ReactBootstrap "SplitButton")))

(def MenuItem (reagent/adapt-react-class (obj/get js/ReactBootstrap "MenuItem")))

(def Pagination (reagent/adapt-react-class (obj/get js/ReactBootstrap "Pagination")))

(def Overlay (reagent/adapt-react-class (obj/get js/ReactBootstrap "Overlay")))
(def OverlayTrigger (reagent/adapt-react-class (obj/get js/ReactBootstrap "OverlayTrigger")))
(def Popover (reagent/adapt-react-class (obj/get js/ReactBootstrap "Popover")))

(def Glyphicon (reagent/adapt-react-class (obj/get js/ReactBootstrap "Glyphicon")))

(def Panel (reagent/adapt-react-class (obj/get js/ReactBootstrap "Panel")))
(def PanelGroup (reagent/adapt-react-class (obj/get js/ReactBootstrap "PanelGroup")))


(def Modal (reagent/adapt-react-class (obj/get js/ReactBootstrap "Modal")))
(def Modal.Dialog (reagent/adapt-react-class (-> js/ReactBootstrap (obj/get "Modal") (obj/get "Dialog"))))
(def Modal.Header (reagent/adapt-react-class (-> js/ReactBootstrap (obj/get "Modal") (obj/get "Header"))))
(def Modal.Title (reagent/adapt-react-class (-> js/ReactBootstrap (obj/get "Modal") (obj/get "Title"))))
(def Modal.Body (reagent/adapt-react-class (-> js/ReactBootstrap (obj/get "Modal") (obj/get "Body"))))
(def Modal.Footer (reagent/adapt-react-class (-> js/ReactBootstrap (obj/get "Modal") (obj/get "Footer"))))

(def Tabs (reagent/adapt-react-class (obj/get js/ReactBootstrap "Tabs")))
(def Tab (reagent/adapt-react-class (obj/get js/ReactBootstrap "Tab")))

(defn popup
  [settings title component]
  (let [str? (string? title)
        id (if str? title (hash title))]
    [OverlayTrigger
     (-> {:placement "bottom"}
         (merge settings)
         (assoc :overlay (reagent/as-element
                          [Popover {:title (when str? title) :id id}
                           (if (vector? component) component [component])])
                :root-close true
                :trigger "click"))
     (if str?
       [Button title [:i.caret]]
       title)]))
