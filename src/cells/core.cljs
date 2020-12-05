(ns cells.core
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [cells.views :as views]))

(defn mount-root []
  (rd/render [views/grid 100 100]
             (js/document.getElementById "app-root")))

