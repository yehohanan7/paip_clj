(ns paip.gps
  (:use [clojure.set]))

(def school-ops
 [ {:action "drive-son-to-school"
    :preconds #{ "son-at-home" "car-works"}
    :add-list #{ "son-at-school"}
    :del-list #{ "son-at-home"}}
   {:action "shop-installs-battery"
    :preconds #{"car-needs-battery" "shop-knows-problem" "shop-has-money"}
    :add-list #{"car-works"}}
   {:action "tell-shop-problem"
    :preconds #{"in-communication-with-shop"}
    :add-list #{"shop-knows-problem"}}
   {:action "telephone-shop"
    :preconds #{"know-phone-number"}
    :add-list #{"in-communication-with-shop"}}
   {:action "look-up-number"
    :preconds #{"have-phone-book"}
    :add-list #{"know-phone-number"}}
   {:action "give-shop-money"
    :preconds #{"have-money"}
    :add-list #{"shop-has-money"}
    :del-list #{"have-money"}}])


(def ^:dynamic *ops* school-ops)

(declare achieve)

(defn appropriate? [goal op]
  (contains? (:add-list op) goal))

(defn achieve-all [func state goals]
  (reduce #(if (nil? %1)
             nil
             (func %1 %2)) state goals))

(defn apply-op [current-state op]
  (println "applying" op " to " current-state)
  (let [new-current-state (achieve-all achieve current-state (:preconds op))]
    (if (nil? new-current-state)
      nil
      (do (println (str "executing " (:action op)))
          (-> new-current-state (difference (:del-list op)) (union (:add-list op)))))))


(defn achieve
  "return the new-state after the goal is achieved or nil
      if it could not be archieved"
  [current-state goal]
  (if (contains? current-state goal)
    current-state
    (some (partial apply-op current-state)
          (filter #(appropriate? goal %) *ops*))))


(defn GPS [state goals]
  (let [new-state (achieve-all achieve state goals)]
    (if (nil? new-state)
      'not-solved
      'solved)))


