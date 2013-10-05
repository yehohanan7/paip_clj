(ns paip.eliza
  (:use [clojure.set]))

(comment 
(defn simple-equal? [x y]
  (cond 
   (or (symbol? x) (symbol? y)) (= x y)
   (or (empty? x) (empty? y)) true
   :else (and (simple-equal? (first x) (first y)) 
              (simple-equal? (rest x) (rest y)))))
)

(def simple-equal? =)

(defn variable? "Is x a variable (a symbol starting with ?) ?" [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn not-matchable? [x y]
  (let [not-sequential? (comp sequential?)]
    (or (and (sequential? x) (not-sequential? y))
        (and (sequential? y) (not-sequential? x)))))

(defn pat-match "Does pattern match input? " [pattern input]
  (cond 
   (not-matchable? pattern input) '()
   (variable? pattern) true
   (and (symbol? input) (symbol? pattern)) (= input pattern)
   :else (and (pat-match (first pattern) (first input))
              (pat-match (rest pattern) (rest input)))))




