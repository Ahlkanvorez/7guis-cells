(ns cells.styles)

(def grid-cell {:border-right "1px solid #D3D3D3"
                :border-bottom "1px solid #D3D3D3"
                :min-width "60px"
                :min-height "20px"})

(def cell (merge-with merge grid-cell {:display :grid :place-items :stretch}))

(defn input-cell [selected?]
  (let [border (if selected? "1px solid black" :none)]
    (merge grid-cell
           {:display :grid
            :overflow :auto
            :margin 0
            :outline :none
            :padding (if selected? 1 2)
            :border-bottom border
            :border-top border
            :border-right border
            :border-left border})))

(defn header [direction]
  (merge grid-cell
         {:padding 2
          :font-weight :bold
          :background-color "#D3D3D3"
          :place-content :center
          :overflow :auto
          :display :grid
          :resize direction}))

(defn grid [rows cols]
  {:display :grid
   :grid-template-columns (str "repeat(" cols ", minmax(min-content, max-content))")
   :grid-template-rows (str "repeat(" rows ", minmax(min-content, max-content))")
   :place-items :stretch})