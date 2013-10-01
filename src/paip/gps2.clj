(ns paip.gps2
  (:use [clojure.set]))

(def ^:dynamic *dbg-ids* #{:gps})

(defn debug
  ([id str] (debug id 0 str))
  ([id indent str]
     (when (contains? *dbg-ids* id)
       (do (dotimes [i indent]
             (print " "))
           (println str)))))

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

(defn appropriate-ops-for [goal] 
  (filter #(appropriate? goal %) *ops*))

(defn achieve-all [func state goals]
  (reduce #(if (nil? %1)
             nil
             (func %1 %2)) state goals))


(defn apply-op [goal-stack goal state op]
  (debug :gps (count goal-stack) (str "Consider: " (:action op)))
  (let [[new-current-state new-list-of-actions] (achieve-all (partial achieve (conj goal-stack goal))
                                                             state (:preconds op))]
    (if (nil? new-current-state)
      [nil new-list-of-actions]
      (do (debug :gps (count goal-stack) (str "Action " (:action op)))
          [(-> new-current-state (difference (:del-list op)) (union (:add-list op)))
           (conj new-list-of-actions (:action op))]))))


(defn achieve
  "return the new-state after the goal is achieved or nil
      if it could not be archieved"
     [goal-stack [current-state list-of-actions] goal]
     (debug :gps (count goal-stack) (str "Goal " goal))
     (cond (contains? current-state goal) [current-state list-of-actions]
           (contains? goal-stack goal) [nil list-of-actions]
           :else (some (partial apply-op goal-stack goal [current-state list-of-actions])
                       (appropriate-ops-for goal))))


(defn GPS [state goals]
  (let [[new-state list-of-actions] (achieve-all (partial achieve []) [state []] goals)]
    (if (nil? new-state)
      nil
      list-of-actions)))


