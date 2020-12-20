(ns cells.views
  (:require [reagent.core :as r]
            [cells.grid :as grid]
            [cells.styles :as styles]))

(defn cell [_options contents]
  [:div {:style styles/cell} contents])

(defn input-cell [{:keys [row col register-value blur-handler]}]
  (let [value (r/atom {:formula "" :value "" :selected false})
        _ (register-value [row col] value)
        on-blur (blur-handler [row col])]
    (fn [{:keys [register-value blur-handler]}]
      [:input
       {:style (styles/input-cell (:selected @value))
        :type :text
        :auto-focus true
        :value (if (:selected @value)
                 (:formula @value)
                 (:value @value))
        :on-change #(when (:selected @value)
                      (swap! value assoc
                             :formula (.. % -target -value)
                             :invalidated true))
        :on-blur
        (fn [_e]
          (if (:invalidated @value)
            (do (swap! value assoc :selected false :invalidated false)
                (on-blur value))
            (swap! value assoc :selected false)))
        :on-double-click #(swap! value assoc :selected true)}])))

(defn header [direction content]
  [:div {:style (styles/header direction)} content])

(defn col-header [options content] (header :horizontal content))
(defn row-header [options content] (header :vertical content))

(defn grid [{:keys [rows cols value-registrar make-blur-handler]}]
  (let [cols (inc cols)]
    (letfn [(row [idx] (int (/ idx cols)))
            (col [idx] (mod idx cols))
            (col-header? [idx] (= 0 (row idx)))
            (row-header? [idx] (= 0 (col idx)))]
      [:div {:style (styles/grid rows cols)}
       (doall
        (for [i (range (* rows cols))]
          (cond
            (= i 0) [:div {:key i :style styles/cell}
                     [:a {:href "https://bitbucket.org/ahlk/cells-cljs"
                          :target "_blank"
                          :style {:text-decoration :none}}
                      "(->source)"]]
            (col-header? i) [col-header {:key i} (grid/int->uppercase (dec i))]
            (row-header? i) [row-header {:key i} (/ i cols)]
            :default
            [cell {:key i}
             [input-cell {:row (row i)
                          :col (col i)
                          :register-value value-registrar
                          :blur-handler make-blur-handler}]])))])))
