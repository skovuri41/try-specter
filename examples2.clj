(ns try-specter.examples2
  (:require [com.rpl.specter :as sp]))

;; Intro

(def flat-numbers [1 2 3 4])

(defn inc-flat-numbers
  "Increment each number"
  []
  (map inc flat-numbers))

(defn inc-flat-numbers-specter []
  (sp/transform [sp/ALL] inc flat-numbers))

(def maps-numbers [{:id 1 :value 1} {:id 2 :value 2} {:id 3 :value 3}])

(defn inc-maps-numbers
  "Increment each :value"
  []
  (map #(update % :value inc) maps-numbers))

(defn inc-maps-numbers-specter []
  (sp/transform [sp/ALL :value] inc maps-numbers))

(def nested-numbers {:a [{:aa 1 :bb 2}
                         {:cc 3}]
                     :b [{:dd 4}]})

(defn inc-even-nested-numbers-specter []
  (sp/transform [sp/MAP-VALS sp/ALL sp/MAP-VALS even?] inc nested-numbers)
  )

(defn inc-even-nested-numbers
  "Increment every even number nested within a map of vector of maps"
  []
  (letfn [(map-vals [m afn]
            (->> m
                 (map (fn [[k v]] [k (afn v)]))
                 (into (empty m))))]
    (map-vals nested-numbers
              (fn [v]
                (mapv ;; mapv
                 (fn [m]
                   (map-vals
                    m
                    (fn [v] (if (even? v) (inc v) v))))
                 v)))))

;; Specter Powers

(defn update-in-on-steroids []
  (update-in {:a {:b {:c 1}}} [:a :b :c] #(if (even? %) (inc %) %))
  (sp/transform [:a :b :c even?] inc {:a {:b {:c 1}}}))

(defn precise-extraction []
  (sp/select [sp/MAP-VALS sp/ALL sp/MAP-VALS even?] nested-numbers))

(defn reusable-paths []
  (let [even-numbers-path [sp/MAP-VALS sp/ALL sp/MAP-VALS even?]]
    (sp/transform even-numbers-path inc nested-numbers)
    (sp/select even-numbers-path nested-numbers)))

;; Specter Super Power

(def tree [{:id       1
            :label    "Freinage"
            :children [{:id       11
                        :label    "Plaquettes"
                        :children [{:id 111 :label "Plaquette de frein"}
                                   {:id 112 :label "Accessoires de plaquette de frein"}]}
                       {:id       12
                        :label    "Disques"
                        :children [{:id 121 :label "Disque de frein"}
                                   {:id 122 :label "Flasque de frein"}]}]}
           {:id       2
            :label    "Pices Moteur et Huile"
            :children [{:id       21
                        :label    "Courroies et Distribution"
                        :children [{:id 211 :label "Kit de distribution"}
                                   {:id 212 :label "Courroie de distribution"}]}]}
           {:id    3
            :label "Gloubi Boulga"}])

(def LEAVES
  "Extract leaf nodes from a tree using schema v4 :leaf attribute"
  (sp/recursive-path [] p [sp/ALL (sp/if-path (sp/must :children)
                                              [:children p]
                                              sp/STAY)]))


(comment
  (sp/select LEAVES tree)

  (sp/setval [LEAVES :leaf] true tree))

(def by-id
  "Specter navigator for trees.
   Retrieves the node identified by the `:id` passed in parameter.
   Ex: (sp/select-first (by-id 1) tree)"
  (sp/recursive-path [id] p
                     [sp/ALL (sp/if-path #(= id (:id %))
                                         sp/STAY
                                         [:children p])]))

(comment
  (sp/select-one (by-id 211) tree))

(def by-ids
  "Specter navigator for trees.
   Retrieves the node identified by its `:id` from the ids passed in parameter.
   Ex: (sp/select-first (by-ids [1 2]) tree)"
  (sp/recursive-path [ids] p
                     [sp/ALL (sp/if-path (fn [node] (some #(= % (:id node)) ids))
                                         (sp/continue-then-stay :children p)
                                         [:children p])]))

(comment
  (sp/select (by-ids [2 211]) tree)) ;; Bug with sp/STAY, can you find it ?

(defn group-leaves-by-parent
  "Group leaves by each top level parent node of a tree"
  [tree]
  (sp/transform [sp/ALL (sp/collect :children LEAVES)]
                (fn group [leaves parent]
                  (if (seq leaves) (assoc parent :children leaves)
                      sp/NONE))
                tree))

(sp/select [sp/ALL (sp/collect :children LEAVES)] tree)

(comment
  (group-leaves-by-parent tree))
