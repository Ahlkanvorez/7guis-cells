(ns cells.formula
  (:require [clojure.string :as string]
            [cells.grid :as grid]))

(defn parse-function [formula]
  (loop [equation (seq formula)
         stack ()
         s ""]
    (if-let [c (first equation)]
      (if (contains? #{"(" ")" ","} c)
        (recur (rest equation)
               (let [trimmed (string/trim s)]
                 (if-not (empty? trimmed)
                   (conj stack trimmed)
                   stack))
               "")
        (recur (rest equation) stack (str s c)))
      stack)))

(defn formula-type [formula]
  (cond (grid/cell-range formula) :range
        (grid/cell-ref? formula) :reference
        (seq (parse-function formula)) :function
        :else :error))

(defmulti parse-formula (fn [_values formula] (formula-type formula)))

(defmethod parse-formula :error [_values _formula]
  {:refresh (fn [] "#Error") :deps []})

(defmethod parse-formula :reference [values formula]
  (let [cell (grid/cell-for-ref @values formula)]
    {:refresh #(:value @cell) :deps [(grid/ref->coord formula)]}))

(defmethod parse-formula :range [values formula]
  (let [[start end] (rest (grid/cell-range formula))
        start (grid/ref->coord start)
        end (grid/ref->coord end)
        dep-coords (grid/coord-grid start end)
        dep-atoms (map (partial get @values) dep-coords)]
    {:deps dep-coords
     :refresh #(let [dep-vals (map (comp :value deref) dep-atoms)]
                 (apply str dep-vals))}))

(defn parse-args [args]
  (if (string? args)
    (js/parseFloat args)
    (->> (seq args) (remove nil?) (remove empty?) (map js/parseFloat))))

(def operations
  (let [op (fn [f]
             (fn [args]
               (->> args flatten parse-args (apply f) str)))]
    {"add" (op +) "sum" (op +)
     "mul" (op *) "prod" (op *)
     "sub" (op -)
     "div" (op /)}))

(defn evaluate-formula [formula]
  (loop [stack () unseen (seq formula)]
    (if (and (empty? unseen) (= 1 (count stack)))
      (first stack)
      (if-let [op (get operations (first stack))]
        (if (and (vector? (second stack)) (> (count (second stack)) 1))
          (recur (conj (drop 2 stack) (op (second stack))) unseen)
          (recur (conj (drop 3 stack) (op (take 2 (rest stack)))) unseen))
        (if-let [n (first unseen)]
          (recur (conj stack n) (rest unseen))
          "#Error")))))

(defmulti form->coords formula-type)
(defmethod form->coords :range [form] (vec (grid/range->coords form)))
(defmethod form->coords :reference [form] [(grid/ref->coord form)])
(defmethod form->coords :default [form] form)

(defn inject-values [vars]
  (map (fn [form]
         (if (vector? form)
           (mapv (comp str (partial get vars)) form)
           form))))

(defn formula-deps [transduce-stack]
  (let [->refs (comp (filter grid/cell-ref?) (map grid/ref->coord))
        ->ranges (comp (filter grid/cell-range) (mapcat grid/range->coords))]
    (concat (transduce-stack ->refs) (transduce-stack ->ranges))))

(defmethod parse-formula :function [values formula]
  (let [stack (parse-function formula)
        transduce-stack #(transduce % conj stack)
        deps (formula-deps transduce-stack)
        vars (->> deps
                  (map (fn [coord] {coord (get @values coord)}))
                  (apply merge))
        deref-vars #(reduce (fn [m [k v]] (assoc m k (:value @v)))
                            {} vars)
        expand-formula (fn []
                         (transduce-stack
                          (comp (map form->coords)
                                (inject-values (deref-vars)))))]
    {:deps deps :refresh #(evaluate-formula (expand-formula))}))

(defmethod parse-formula :default [_values formula]
  {:value formula :deps []})

(defn parse [values s]
  (case (first s)
    \= (assoc (parse-formula values (subs s 1)) :type :formula)
    nil {:type :empty :refresh (fn [] "") :deps []}
    {:type :value :refresh (fn [] s) :deps []}))
