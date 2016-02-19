(ns gt1-ui.bootstrap
  (:require [reagent.core :as reagent]
            cljsjs.react-bootstrap))

(def Button (reagent/adapt-react-class (aget js/ReactBootstrap "Button")))

(def ProgressBar (reagent/adapt-react-class (aget js/ReactBootstrap "ProgressBar")))

(def DropdownButton (reagent/adapt-react-class (aget js/ReactBootstrap "DropdownButton")))

(def MenuItem (reagent/adapt-react-class (aget js/ReactBootstrap "MenuItem")))
