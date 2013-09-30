(ns paip.core
  (:use [clojure.set]))

(defn member? 
  "true if seq contains elm"
  [elem seq]  
  (some #(= elem %) seq))

(defn as-atom [xs]
  (atom (into #{} xs)))

(def ^:dynamic state "current state of GPS" (atom #{}))

(def ^:dynamic ops "list of operations" (atom '()))

(defrecord Op [action preconds add-list del-list])


(defn GPS "General propblem solver, achive all goals using ops" [current-state goals operations]
  (binding [state (as-atom current-state) ops (as-atom operations)]
    (if (every? achieve goals) "solved")))
  

(defn achieve [goal]
  (or (member? goal @state)
      (some apply-op! (filter #(appropriate? goal %1) @ops))))

(defn appropriate? "An op is appropriate to a goal if it is in its add-list" [goal op]
  (member? goal (:add-list op)))


(defn apply-op! [op]
  (if (every? achieve (:preconds op))
    (do 
      (println "executing " (:action op))
      (reset! state (difference @state (:del-list op)))
      (reset! state (union @state (:add-list op)))
      true)
    false))