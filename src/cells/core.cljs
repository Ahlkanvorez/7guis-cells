(ns cells.core
  (:require [reagent.dom :as rd]
            [cells.db :as db]
            [cells.grid :as grid]
            [cells.views :as views]))

(defn main [state]
  [views/grid {:rows (:rows state)
               :cols (:cols state)
               :value-registrar (db/value-registrar state)
               :make-blur-handler (db/value-recalculator state)}])

(defn mount-root []
  (let [state (db/initial-state 12 26)]
    (rd/render [main state]
               (js/document.getElementById "app-root"))))
