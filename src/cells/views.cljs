(ns cells.views)

(def grid-cell-style {:border-right "1px solid #D3D3D3"
                      :border-bottom "1px solid #D3D3D3"
                      :min-width "30px"
                      :min-height "20px"
                      :padding 2})

(defn cell [options contents]
  [:div (merge options {:style grid-cell-style})
   contents])

(defn col-header [options content]
  [:div {:style (merge-with merge grid-cell-style
                            {:font-weight :bold
                             :background-color "#D3D3D3"
                             :text-align :center})}
   content])

(defn row-header [options content]
  [:div {:style (merge-with merge grid-cell-style
                            {:font-weight :bold
                             :background-color "#D3D3D3"
                             :text-align :right})}
   content])

(defn int->base26 [i]
  (loop [digits [(inc (mod i 26))]
         i (int (/ i 26))]
    (if (<= i 0)
      (reverse digits)
      (recur (conj digits (mod i 26))
             (int (/ i 26))))))

(defn int->uppercase [i]
  (letfn [(int->letter [c] (char (+ 65 (dec c))))]
    (->> i int->base26 (map int->letter) (apply str))))

(defn grid [rows cols]
  (let [cols (inc cols)]
    [:div {:style {:display :grid
                   :grid-template-columns (str "repeat(" cols ", minmax(min-content, max-content))")
                   :grid-template-rows (str "repeat(" rows ", minmax(min-content, max-content))")
                   :place-items :stretch}}
     (for [i (range (* rows cols))]
       (cond
         (= i 0) [cell {:key i} nil]
         (< i cols) [col-header {:key i} (int->uppercase (dec i))]
         (= 0 (mod i cols)) [row-header {:key i} (/ i cols)]
         :default [cell {:key i} nil]))]))
