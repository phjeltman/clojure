(ns knuthx.core
  (:require [clojure.string :refer [split capitalize]]
            [clojure.set :refer [union]]))


(def testmatrix
  '((true  false false true  false false true)
    (true  false false true  false false false)
    (false false false true  true  false true)
    (false false true  false true  true  false)
    (false true  true  false false true  true)
    (false true  false false false false true)))


(defn in?
  "Seems like a function like this should have been included in the base language.."
  [l e]
  (some #(= l %) e))


(defn my-transpose [m]
  "Gives the transpose of matrix m."
  (apply map vector m))


(defn count-true [m]
  "Counts the number of occurences of true in the columns of matrix m."
  (map #(if-let [c ((frequencies %) true)]
          c
          0)
       (my-transpose m)))


(defn filter-by-first [l1 l2]
  "Like clojure.set/difference for lists."
  (filter #(not (in? % l2)) l1))


(defn remove-from-matrix [m rows columns]
  "rows is a list of indices of rows to be removed in m.
  columns is a list of indices of columns to be removed in m."
  (remove empty?
          (map #(map (fn [x] (nth % x))
                     (filter-by-first (range 0 (count (first m)))
                                      columns))
               (map (fn [x] (nth m x))
                    (filter-by-first (range 0 (count m))
                                     rows)))))


(defn get-indices-where-true [m index mode]
  "Returns the indices of the rows/columns that are truthy at columns/row index."
  (let [ma (if (= mode :row)
             (my-transpose m)
             m)]
    (map second
         (filter #(= true (first %))
                 (map vector
                      (nth ma index) (iterate inc 0))))))


(defn submatrix [m row]
  "Helper function to generate the submatrixes for knuth-x."
  (let [cols (get-indices-where-true m row :column)
        rows (filter-by-first (distinct (apply concat (map #(get-indices-where-true m % :row) cols))) (list row))
        red (remove-from-matrix m rows cols)]
    (vector row (sort rows) red)))


(defn pseudo-flatten
  "Walks the call tree to extract the solutions."
  [x]
  (filter #(and (sequential? %) (not-any? sequential? %))
          (rest (tree-seq #(and (sequential? %) (some sequential? %)) seq x))))


(defn knuth-x
  "Implementation of Donald Knuth's 'Algorithm X' based on the Wiki-page http://en.wikipedia.org/wiki/Knuth's_Algorithm_X"
  ([m] (remove empty?
               (pseudo-flatten
                (knuth-x m [] (map list
                                   (range (count m)))))))
  ([m res rows]
   (cond (empty? m)
         res
         (nil? m)
         nil
         :else
         (let [column-count (count-true m)
               min-count (if (empty? column-count)
                           0
                           (apply min column-count))]
           (let [mincol-idx (ffirst (filter (fn [[a b]] (= b min-count))
                                            (map vector
                                                 (iterate inc 0)
                                                 column-count)))]
             (when mincol-idx
               (map (fn [[re r m]]
                      (knuth-x m
                               (conj res (nth (flatten rows) re))
                               (remove-from-matrix rows
                                                   r
                                                   '())))
                    (map #(submatrix m %)
                         (get-indices-where-true m mincol-idx :row)))))))))


(knuth-x (shuffle testmatrix))




