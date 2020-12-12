(ns cells.db
  (:require [reagent.core :as r]
            [cells.formula :as formula]))

(defn refresher
  "Return a fn to refresh all cells dependant on this cell's value.
  The returned fn becomes invalid if the cell's formula changes."
  [{:keys [values deps]}]
  (fn [coord]
    (doseq [dep-coord (get @deps coord)]
      (let [dep-val (get @values dep-coord)]
        (swap! dep-val assoc :value ((:refresh @dep-val) dep-coord))))))

(defn recalculator
  "Return a fn which recalculates the cell at `coord`'s value.
  The formula is parsed each invocation, the cell's dependencies are
  registered, and any cells which depend on the value of this cell
  are refreshed."
  [{:keys [values deps] :as db} coord]
  (let [refresh-deps (refresher db)]
    (fn [val]
      (let [parsed (formula/parse values (:formula @val))
            refresh-cell (:refresh parsed)]
        (if (contains? (set (:deps parsed)) coord)
          (swap! val assoc :refresh (fn [] "#Error!") :value "#Error!")
          (do
            (doseq [dep (:deps parsed)]
              (swap! deps update dep #(conj (or % #{}) coord)))
            (let [do-refresh (fn [coord]
                               (let [new-value (refresh-cell)]
                                 (swap! val assoc :value new-value)
                                 (refresh-deps coord)
                                 new-value))]
              (swap! val assoc :refresh do-refresh :value (refresh-cell))
              (refresh-deps coord))))))))

(defn recalculate-handler [db coord]
  (let [recalculate (recalculator db coord)]
    (swap! (get @(:values db) coord) assoc :recalculate recalculate)
    recalculate))

(defn initial-state [rows cols]
  {:rows rows :cols cols :values (atom {}) :deps (atom {})})

(defn new-cell-value []
  (r/atom {:formula "" :value "" :selected false}))

(defn value-registrar [db]
  (fn [coord value]
    (swap! (:values db) assoc coord value)))

(defn value-recalculator [db]
  (fn [coord]
    (recalculate-handler db coord)))
