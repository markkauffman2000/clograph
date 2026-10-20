(ns clograph.compiler)

(defn node-sym
  "Converts a keyword id like :val-1 into a Clojure symbol val-1."
  [id]
  (when id
    (symbol (name id))))

(defn incoming-edge [edges to-node to-port]
  (first (filter #(and (= (:to-node %) to-node)
                       (= (:to-port %) to-port))
                 edges)))

(defn downstream-nodes
  "Returns a set of all node IDs reachable downstream from `start-node-id`."
  [edges start-node-id]
  (let [adj (reduce (fn [acc {:keys [from-node to-node]}]
                      (update acc from-node (fnil conj #{}) to-node))
                    {}
                    edges)]
    (loop [frontier (get adj start-node-id #{})
           visited  #{}]
      (if (empty? frontier)
        visited
        (let [curr          (first frontier)
              rest-frontier (disj frontier curr)]
          (if (visited curr)
            (recur rest-frontier visited)
            (recur (into rest-frontier (get adj curr #{}))
                   (conj visited curr))))))))

(defn- node-dependencies [nodes edges]
  (into {}
        (for [{:keys [id inputs]} nodes]
          [id (set (keep (fn [port]
                           (:from-node (incoming-edge edges id port)))
                         inputs))])))

(defn topological-sort
  "Standard Kahn's algorithm: returns nodes in valid execution order."
  [nodes edges]
  (let [node-map (into {} (map (juxt :id identity) nodes))
        deps (node-dependencies nodes edges)]
    (loop [resolved []
           deps deps]
      (let [ready-nodes (map key (filter (comp empty? val) deps))]
        (if (empty? ready-nodes)
          (if (empty? deps)
            (mapv node-map resolved)
            (throw (ex-info "Cycle detected in dataflow graph!" {:remaining deps})))
          (let [next-id (first ready-nodes)
                remaining (into {}
                                (for [[n d] (dissoc deps next-id)]
                                  [n (disj d next-id)]))]
            (recur (conj resolved next-id) remaining)))))))

(defn compile-node-expr
  "Turns a single node into its Clojure binding expression."
  [node edges]
  (case (:type node)
    :val
    (:value node)

    :fn
    (let [arg-bindings (mapv (fn [p]
                               (if-let [e (incoming-edge edges (:id node) p)]
                                 (node-sym (:from-node e))
                                 (throw (ex-info (str "Unconnected input port: " (name p) " on " (name (:id node)))
                                                 {:node (:id node) :port p}))))
                             (:inputs node))]
      `(~(:op node) ~@arg-bindings))

    :output
    (if-let [e (incoming-edge edges (:id node) (first (:inputs node)))]
      (node-sym (:from-node e))
      nil)))

(defn graph->clojure
  "Compiles the DAG into a valid Clojure (let [sym expr ...] {...}) form."
  [{:keys [nodes edges]}]
  (let [sorted-nodes (topological-sort nodes edges)
        bindings (vec (mapcat (fn [n]
                                [(node-sym (:id n)) (compile-node-expr n edges)])
                              sorted-nodes))
        output-nodes (filter #(= :output (:type %)) nodes)
        result-map (into {} (map (fn [o] [(:id o) (node-sym (:id o))]) output-nodes))]
    `(let [~@bindings]
       ~result-map)))

(defn run-graph
  "Compiles and evaluates the graph in memory, returning {:out-1 42, ...}."
  [graph]
  (let [code (graph->clojure graph)]
    (println "\n[CloGraph Generated AST]:" (pr-str code))
    (eval code)))