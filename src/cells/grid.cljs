(ns cells.grid
  (:require [clojure.string :as string]))

(defn int->base26-digits [i]
  (loop [digits [(inc (mod i 26))]
         i (int (/ i 26))]
    (if (<= i 0)
      (reverse digits)
      (recur (conj digits (mod i 26))
             (int (/ i 26))))))

(defn base26-digits->int [digits]
  (loop [i 0
         e 1
         digits digits]
    (if (empty? digits)
      (int i)
      (recur (+ i (* (first digits) e))
             (* 26 e)
             (rest digits)))))

(defn uppercase->int [s]
  (letfn [(letter->digit [c] (inc (- (int (.charCodeAt c)) 65)))]
    (->> (map letter->digit s) reverse base26-digits->int)))

(defn int->uppercase [i]
  (letfn [(digit->letter [c] (char (+ 65 (dec c))))]
    (->> i int->base26-digits (map digit->letter) (apply str))))

(defn ref->coord [ref]
  (let [[_match col row] (re-find #"([A-Z]+)([0-9]+)" ref)]
    [(int row) (uppercase->int col)]))

(defn coord->ref [[row col]]
  (str (int->uppercase col) row))

(def cell-ref? (partial re-find #"^[A-Z]+[0-9]+$"))
(def cell-range (partial re-find #"^([A-Z]+[0-9]+):([A-Z]+[0-9]+)$"))

(defn cell-for-ref [values ref]
  (when-let [coord (ref->coord ref)]
    (get values coord)))

(defn coord-grid [[row-start col-start] [row-end col-end]]
  (for [r (range row-start (inc row-end))
        c (range col-start (inc col-end))]
    [r c]))

(defn range->coords [range]
  (let [[start-ref end-ref] (string/split range #":")]
    (coord-grid (ref->coord start-ref) (ref->coord end-ref))))
