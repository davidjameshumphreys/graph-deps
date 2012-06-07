(ns dojo-coding-1.core
  (:use [midje.sweet]))

(def EMPTY {:nodes #{} :edges []})

(defn- add-info [{:keys [nodes edges] :or [#{} []] :as current-map}
                 node-name
                 node-edges]

  {:nodes (conj nodes node-name)
   :edges (vec (concat edges (map (fn [a] [node-name a]) node-edges)))}
  )

(fact
 (add-info
  {:nodes #{} :edges []} "node1" #{"node2"}) =>
  {:nodes #{"node1"} :edges [["node1" "node2"]]})

(fact
 (add-info EMPTY "node1" []) => {:nodes #{"node1"} :edges []})

(fact
 (add-info
  (add-info EMPTY :node1 #{:node2})
  :node2
  #{:node1 :node3})
 =>
 {:nodes #{:node1 :node2},
     :edges [[:node1 :node2] [:node2 :node1] [:node2 :node3]]})

(defn map-lines
  [m]
  (map identity m))

(defn build-lines ""
  [mp]
  (loop [current-map EMPTY
         [k & kothers] (keys mp)]
    (if (nil? k)
      current-map
      (recur
       (add-info current-map k (get mp k))
       kothers))))
(defn graph-lines ""
  [m]
  (let [output (build-lines m)]
    (concat
     ["digraph fn {"]
     (map (fn [a] (str \" a \" \;)) (:nodes output))

     (map (fn [ [na nb] ] (str \" na "\" -> \"" nb \" \;)) (:edges output))
     ["}"])
    ))

(fact
  (graph-lines {"node1" #{"node2"}
                "node2" #{"node1" "node3"}
                "node3" #{}}) => (just
                                  ["digraph fn {"
                                   "\"node1\";"
                                   "\"node2\";"
                                   "\"node3\";"
                                   "\"node1\" -> \"node2\";"
                                   "\"node2\" -> \"node1\";"
                                   "\"node2\" -> \"node3\";"
                                   "}"] :in-any-order))
