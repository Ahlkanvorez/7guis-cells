(ns cells.formula
  (:require [cells.grid :as grid]))

(defn parse-function [formula]
  (loop [equation (seq formula)
         stack ()
         s ""]
    (if-let [c (first equation)]
      (if (contains? #{"(" ")" ","} c)
        (recur (rest equation)
               (let [trimmed (clojure.string/trim s)]
                 (if-not (empty? trimmed)
                   (conj stack trimmed)
                   stack))
               "")
        (recur (rest equation) stack (str s c)))
      stack)))

(defn formula-type [formula]
  (cond (grid/cell-range formula) :range
        (grid/cell-ref? formula) :reference
        (not (empty? (parse-function formula))) :function
        :default :error))

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

(def operations
  (let [op (fn [f] (fn [args] (str (apply f (reverse args)))))]
    {"add" (op +) "sum" (op +)
     "mul" (op *) "prod" (op *)
     "sub" (op -)
     "div" (op /)}))

(defn evaluate-formula [variables stack]
  (loop [args () stack stack]
    (if (and (empty? args) (= 1 (count stack)))
      (first stack)
      (if-let [op (get operations (first stack))]
        (recur () (conj (rest stack) (op args)))
        (recur (let [v (first stack)]
                 (cond (empty? v) args
                       (string? v) (conj args (js/parseFloat v))
                       (vector? v) (concat args
                                           (->> (remove empty? v)
                                                (map js/parseFloat)))
                       :default args))
               (rest stack))))))

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
    {:deps deps :refresh #(evaluate-formula vars (expand-formula))}))

(defmethod parse-formula :default [_values formula]
  {:value formula :deps []})

(defn parse [values s]
  (case (first s)
    \= (assoc (parse-formula values (subs s 1)) :type :formula)
    nil {:type :empty :refresh (fn [] "") :deps []}
    {:type :value :refresh (fn [] s) :deps []}))
