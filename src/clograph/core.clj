(ns clograph.core
  (:require [cljfx.api :as fx]
            [clograph.compiler :as comp]
            [clojure.string :as str])
  (:import [javafx.scene.control ContextMenu MenuItem]
           [javafx.scene.input MouseButton ContextMenuEvent MouseEvent])
  (:gen-class))

;; --- Geometry & Initial State ---

(def node-width 140.0)
(def node-height 75.0)

(def initial-state
  {:nodes
   [{:id :val-1 :type :val    :value 15  :x 160.0 :y 50.0  :outputs [:out]}
    {:id :val-2 :type :val    :value 27  :x 380.0 :y 50.0  :outputs [:out]}
    {:id :fn-1  :type :fn     :op '+     :op-text "+" :editing? false :x 270.0 :y 190.0 :inputs [:in-1 :in-2] :outputs [:out]}
    {:id :out-1 :type :output :result nil :x 270.0 :y 340.0 :inputs [:in]}]

   :edges
   [{:from-node :val-1 :from-port :out :to-node :fn-1 :to-port :in-1}
    {:from-node :val-2 :from-port :out :to-node :fn-1 :to-port :in-2}
    {:from-node :fn-1  :from-port :out :to-node :out-1 :to-port :in}]

   :connecting nil
   :drag-state nil
   :status "Ready. Click function name to edit, or right-click for options."})

(defonce *state (atom initial-state))

;; --- Port Geometry Helpers ---

(defn port-x [node port-idx total-ports]
  (let [spacing (/ node-width (inc total-ports))]
    (+ (:x node) (* (inc port-idx) spacing))))

(defn port-coord [nodes node-id port-id kind]
  (when-let [node (first (filter #(= (:id %) node-id) nodes))]
    (let [ports (if (= kind :in) (:inputs node) (:outputs node))
          idx (.indexOf ports port-id)
          x (port-x node (max 0 idx) (max 1 (count ports)))
          y (if (= kind :in) (:y node) (+ (:y node) node-height))]
      [x y])))

;; --- Context Menu Helper ---

(defn show-context-menu! [^ContextMenuEvent e items]
  (let [cm (ContextMenu.)
        menu-items (mapv (fn [{:keys [text on-action]}]
                           (doto (MenuItem. text)
                             (.setOnAction (reify javafx.event.EventHandler
                                             (handle [_ _] (on-action))))))
                         items)]
    (.setAll (.getItems cm) (into-array MenuItem menu-items))
    (.show cm ^javafx.scene.Node (.getSource e) (.getScreenX e) (.getScreenY e))
    (.consume e)))

;; --- Central Event Handler ---

(defn handle-event [{:keys [event/type] :as event}]
  (case type
    :add-node
    (let [ntype (:node-type event)
          id (keyword (str (name ntype) "-" (rand-int 1000)))
          new-node (case ntype
                     :val    {:id id :type :val :value 10 :x 100.0 :y 100.0 :outputs [:out]}
                     :fn     {:id id :type :fn  :op nil :op-text "" :editing? true :inputs [] :outputs [:out] :x 100.0 :y 100.0}
                     :output {:id id :type :output :result nil :x 100.0 :y 100.0 :inputs [:in]})]
      (swap! *state (fn [s]
                      (-> s
                          (update :nodes conj new-node)
                          (assoc :status (if (= ntype :fn)
                                           "Added Function node. Type operator and press Enter."
                                           (str "Added " (name ntype) " node.")))))))

    :edit-fn-node
    (let [node-id (:node-id event)]
      (swap! *state update :nodes
             (fn [nodes]
               (mapv (fn [n]
                       (if (= (:id n) node-id)
                         (assoc n :editing? true
                                :op-text (if (:op n) (str (:op n)) ""))
                         n))
                     nodes))))

    :update-fn-text
    (let [{:keys [node-id text]} event]
      (swap! *state update :nodes
             (fn [nodes]
               (mapv (fn [n]
                       (if (= (:id n) node-id)
                         (assoc n :op-text text)
                         n))
                     nodes))))

    :commit-fn-op
    (let [node-id (:node-id event)
          node (first (filter #(= (:id %) node-id) (:nodes @*state)))
          raw-text (or (:op-text node) "")
          trimmed (str/trim raw-text)]
      (if (empty? trimmed)
        (if (:op node)
          (swap! *state update :nodes
                 (fn [nodes]
                   (mapv #(if (= (:id %) node-id) (assoc % :editing? false) %) nodes)))
          (swap! *state assoc :status "Please enter a function name (e.g. +, *, inc)."))
        (let [sym (symbol trimmed)
              resolved-var (comp/resolve-op sym)
              arity (comp/infer-arity resolved-var)
              new-inputs (mapv #(keyword (str "in-" (inc %))) (range arity))
              new-input-set (set new-inputs)]
          (swap! *state
                 (fn [{:keys [nodes edges] :as state}]
                   (let [downstream (comp/downstream-nodes edges node-id)
                         clean-edges (vec (remove #(and (= (:to-node %) node-id)
                                                        (not (new-input-set (:to-port %))))
                                                  edges))]
                     (-> state
                         (assoc :edges clean-edges)
                         (assoc :nodes
                                (mapv (fn [n]
                                        (cond
                                          (= (:id n) node-id)
                                          (assoc n :op sym
                                                 :editing? false
                                                 :inputs new-inputs)

                                          (downstream (:id n))
                                          (assoc n :result nil)

                                          :else n))
                                      nodes))
                         (assoc :status (if resolved-var
                                          (str "Configured '" sym "' (" arity " input ports).")
                                          (str "Configured '" sym "' (defaulted to " arity " inputs)."))))))))))

    :delete-node
    (let [node-id (:node-id event)]
      (swap! *state
             (fn [{:keys [nodes edges] :as state}]
               (let [downstream (comp/downstream-nodes edges node-id)
                     remaining-nodes (vec (remove #(= (:id %) node-id) nodes))
                     remaining-edges (vec (remove #(or (= (:from-node %) node-id)
                                                       (= (:to-node %) node-id))
                                                  edges))]
                 (-> state
                     (assoc :nodes (mapv #(if (downstream (:id %)) (assoc % :result nil) %) remaining-nodes))
                     (assoc :edges remaining-edges)
                     (assoc :connecting nil)
                     (assoc :status (str "Deleted node " (name node-id))))))))

    :add-input-port
    (let [node-id (:node-id event)]
      (swap! *state
             (fn [{:keys [nodes edges] :as state}]
               (let [node (first (filter #(= (:id %) node-id) nodes))
                     existing (set (:inputs node))
                     new-port-id (first (remove existing (map #(keyword (str "in-" %)) (iterate inc 1))))
                     downstream (comp/downstream-nodes edges node-id)]
                 (-> state
                     (assoc :nodes
                            (mapv (fn [n]
                                    (cond
                                      (= (:id n) node-id)
                                      (update n :inputs (fnil conj []) new-port-id)

                                      (downstream (:id n))
                                      (assoc n :result nil)

                                      :else n))
                                  nodes))
                     (assoc :status (str "Added port " (name new-port-id) " to " (name node-id))))))))

    :remove-input-port
    (let [{:keys [node-id port-id]} event]
      (swap! *state
             (fn [{:keys [nodes edges] :as state}]
               (let [downstream (comp/downstream-nodes edges node-id)
                     clean-edges (vec (remove #(and (= (:to-node %) node-id)
                                                    (= (:to-port %) port-id))
                                              edges))]
                 (-> state
                     (assoc :edges clean-edges)
                     (assoc :nodes
                            (mapv (fn [n]
                                    (cond
                                      (= (:id n) node-id)
                                      (update n :inputs (fn [ports] (vec (remove #(= % port-id) ports))))

                                      (downstream (:id n))
                                      (assoc n :result nil)

                                      :else n))
                                  nodes))
                     (assoc :status (str "Removed port " (name port-id))))))))

    :remove-last-input-port
    (let [node-id (:node-id event)
          node (first (filter #(= (:id %) node-id) (:nodes @*state)))
          last-port (last (:inputs node))]
      (when last-port
        (handle-event {:event/type :remove-input-port :node-id node-id :port-id last-port})))

    :update-val
    (let [val     (or (try (Long/parseLong (:val event)) (catch Exception _ nil)) 0)
          node-id (:node-id event)]
      (swap! *state
             (fn [{:keys [edges nodes] :as state}]
               (let [affected (comp/downstream-nodes edges node-id)]
                 (assoc state :nodes
                        (mapv (fn [n]
                                (cond
                                  (= (:id n) node-id)
                                  (assoc n :value val)

                                  (and (affected (:id n)) (= (:type n) :output))
                                  (assoc n :result nil)

                                  :else n))
                              nodes))))))

    :click-output-port
    (swap! *state assoc
           :connecting {:from-node (:node-id event) :from-port (:port-id event)}
           :status (str "Wiring from " (name (:node-id event)) ". Click an input port."))

    :click-input-port
    (swap! *state
           (fn [{:keys [connecting edges nodes] :as state}]
             (if connecting
               (let [target-id (:node-id event)
                     downstream (comp/downstream-nodes edges target-id)
                     new-edge {:from-node (:from-node connecting)
                               :from-port (:from-port connecting)
                               :to-node   target-id
                               :to-port   (:port-id event)}
                     clean-edges (vec (remove #(and (= (:to-node %) target-id)
                                                    (= (:to-port %) (:port-id event)))
                                              edges))]
                 (-> state
                     (assoc :connecting nil)
                     (assoc :edges (conj clean-edges new-edge))
                     (assoc :nodes (mapv #(if (downstream (:id %)) (assoc % :result nil) %) nodes))
                     (assoc :status "Wire connected!")))
               state)))

    :cancel-connecting
    (swap! *state assoc :connecting nil :status "Wiring cancelled.")

    :clear-wires
    (swap! *state (fn [s]
                    (-> s
                        (assoc :edges []
                               :connecting nil
                               :status "Cleared all wires.")
                        (update :nodes (fn [nodes]
                                         (mapv #(if (= (:type %) :output) (assoc % :result nil) %) nodes))))))

    :run-graph
    (try
      (let [results (comp/run-graph @*state)]
        (swap! *state (fn [s]
                        (-> s
                            (assoc :status "Executed successfully.")
                            (update :nodes
                                    (fn [nodes]
                                      (mapv (fn [n]
                                              (if (= (:type n) :output)
                                                (assoc n :result (get results (:id n) "nil"))
                                                n))
                                            nodes)))))))
      (catch Exception ex
        (let [msg (.getMessage ex)]
          (println "[Execution Error]:" msg)
          (swap! *state assoc :status (str "Error: " msg)))))

    :start-drag
    (let [fx-e (:fx/event event)
          node-id (:node-id event)
          node (first (filter #(= (:id %) node-id) (:nodes @*state)))]
      (when (and node (= (.getButton fx-e) MouseButton/PRIMARY))
        (swap! *state assoc :drag-state {:node-id node-id
                                         :mouse-x (.getSceneX fx-e)
                                         :mouse-y (.getSceneY fx-e)
                                         :node-x  (:x node)
                                         :node-y  (:y node)})))

    :drag-node
    (let [fx-e (:fx/event event)
          {:keys [node-id mouse-x mouse-y node-x node-y]} (:drag-state @*state)]
      (when (and node-id mouse-x mouse-y)
        (let [dx (- (.getSceneX fx-e) mouse-x)
              dy (- (.getSceneY fx-e) mouse-y)]
          (swap! *state update :nodes
                 (fn [nodes]
                   (mapv (fn [n]
                           (if (= (:id n) node-id)
                             (assoc n :x (+ node-x dx) :y (+ node-y dy))
                             n))
                         nodes))))))

    :stop-drag
    (swap! *state dissoc :drag-state)

    nil))

;; --- Visual Components ---

(defn render-wire [{:keys [from-node from-port to-node to-port nodes]}]
  (let [[x1 y1] (port-coord nodes from-node from-port :out)
        [x2 y2] (port-coord nodes to-node to-port :in)]
    (if (and x1 y1 x2 y2)
      {:fx/type :cubic-curve
       :start-x x1 :start-y y1
       :end-x x2   :end-y y2
       :control-x1 x1 :control-y1 (+ y1 50.0)
       :control-x2 x2 :control-y2 (- y2 50.0)
       :stroke "#61AFEF"
       :stroke-width 3.0
       :fill :transparent
       :mouse-transparent true}
      {:fx/type :group})))

(defn render-node [{:keys [node connecting]}]
  (let [is-output (= :output (:type node))]
    {:fx/type :group
     :layout-x (:x node)
     :layout-y (:y node)
     :children
     [;; 1. The Card Body
      {:fx/type :v-box
       :pref-width node-width
       :pref-height node-height
       :alignment :center
       :spacing 3
       :on-mouse-pressed {:event/type :start-drag :node-id (:id node)}
       :on-mouse-dragged {:event/type :drag-node}
       :on-mouse-released {:event/type :stop-drag}
       :on-context-menu-requested
       (fn [^ContextMenuEvent e]
         (.consume e)
         (let [items (cond-> []
                       (= :fn (:type node))
                       (into [{:text "Edit Function Name"
                               :on-action #(handle-event {:event/type :edit-fn-node :node-id (:id node)})}
                              {:text "Add Input Port"
                               :on-action #(handle-event {:event/type :add-input-port :node-id (:id node)})}
                              (when (seq (:inputs node))
                                {:text "Remove Last Input Port"
                                 :on-action #(handle-event {:event/type :remove-last-input-port :node-id (:id node)})})])
                       :always
                       (conj {:text "Delete Node"
                              :on-action #(handle-event {:event/type :delete-node :node-id (:id node)})}))]
           (show-context-menu! e (remove nil? items))))
       :style {:-fx-background-color "#282C34"
               :-fx-border-color (if is-output "#98C379" "#4B5263")
               :-fx-border-width 2
               :-fx-border-radius 8
               :-fx-background-radius 8
               :-fx-padding 6
               :-fx-cursor "hand"}
       :children
       (case (:type node)
         :val
         [{:fx/type :label
           :text "VALUE"
           :mouse-transparent true
           :style {:-fx-text-fill "#5C6370" :-fx-font-size 9 :-fx-font-weight "bold"}}
          {:fx/type :text-field
           :pref-width 85.0
           :alignment :center
           :text (str (:value node))
           :style {:-fx-background-color "#1E1E24"
                   :-fx-text-fill "#E5C07B"
                   :-fx-font-size 13
                   :-fx-font-weight "bold"}
           :on-text-changed (fn [txt]
                              (handle-event {:event/type :update-val
                                             :node-id (:id node)
                                             :val txt}))}]

         :fn
         (if (:editing? node)
           [{:fx/type :label
             :text "SET FUNCTION"
             :mouse-transparent true
             :style {:-fx-text-fill "#E5C07B" :-fx-font-size 9 :-fx-font-weight "bold"}}
            {:fx/type :h-box
             :alignment :center
             :spacing 4
             :children
             [{:fx/type :text-field
               :pref-width 74.0
               :prompt-text "+, *, inc"
               :text (or (:op-text node) "")
               :style {:-fx-background-color "#1E1E24"
                       :-fx-text-fill "#61AFEF"
                       :-fx-font-size 12
                       :-fx-font-weight "bold"
                       :-fx-alignment "center"}
               :on-text-changed (fn [txt]
                                  (handle-event {:event/type :update-fn-text
                                                 :node-id (:id node)
                                                 :text txt}))
               :on-action {:event/type :commit-fn-op :node-id (:id node)}}
              {:fx/type :button
               :text "✓"
               :style {:-fx-background-color "#98C379"
                       :-fx-text-fill "#1E1E24"
                       :-fx-font-weight "bold"
                       :-fx-padding "2 6 2 6"
                       :-fx-cursor "hand"}
               :on-action {:event/type :commit-fn-op :node-id (:id node)}}]}]
           [{:fx/type :label
             :text "FUNCTION"
             :mouse-transparent true
             :style {:-fx-text-fill "#5C6370" :-fx-font-size 9 :-fx-font-weight "bold"}}
            {:fx/type :label
             :text (if (:op node) (str (:op node)) "?")
             :style {:-fx-text-fill (if (:op node) "#61AFEF" "#E5C07B")
                     :-fx-font-size 22
                     :-fx-font-weight "bold"
                     :-fx-cursor "hand"}
             :on-mouse-clicked (fn [^MouseEvent e]
                                 (.consume e)
                                 (when (= (.getButton e) MouseButton/PRIMARY)
                                   (handle-event {:event/type :edit-fn-node :node-id (:id node)})))}])

         :output
         [{:fx/type :label
           :text "OUTPUT SINK"
           :mouse-transparent true
           :style {:-fx-text-fill "#98C379" :-fx-font-size 9 :-fx-font-weight "bold"}}
          {:fx/type :label
           :text (str (or (:result node) "---"))
           :mouse-transparent true
           :style {:-fx-text-fill "#FFFFFF"
                   :-fx-font-size 18
                   :-fx-font-weight "bold"}}])}

      ;; 2. Top Input Ports (Green)
      {:fx/type :group
       :children
       (map-indexed
        (fn [idx p]
          (let [px (port-x node idx (count (:inputs node)))]
            {:fx/type :circle
             :center-x (- px (:x node)) :center-y 0.0 :radius 7.5
             :fill (if connecting "#98C379" "#4B5263")
             :stroke "#1E1E24" :stroke-width 2
             :style {:-fx-cursor "crosshair"}
             :on-mouse-pressed (fn [^MouseEvent e] (.consume e))
             :on-mouse-clicked (fn [^MouseEvent e]
                                 (.consume e)
                                 (when (= (.getButton e) MouseButton/PRIMARY)
                                   (handle-event {:event/type :click-input-port
                                                  :node-id (:id node)
                                                  :port-id p})))
             :on-context-menu-requested
             (fn [^ContextMenuEvent e]
               (.consume e)
               (show-context-menu! e [{:text "Delete Port"
                                       :on-action #(handle-event {:event/type :remove-input-port
                                                                  :node-id (:id node)
                                                                  :port-id p})}]))}))
        (:inputs node))}

      ;; 3. Bottom Output Ports (Blue / Gold highlight)
      {:fx/type :group
       :children
       (map-indexed
        (fn [idx p]
          (let [px (port-x node idx (count (:outputs node)))
                is-active (and connecting
                               (= (:from-node connecting) (:id node))
                               (= (:from-port connecting) p))]
            {:fx/type :circle
             :center-x (- px (:x node)) :center-y node-height :radius 7.5
             :fill (if is-active "#E5C07B" "#61AFEF")
             :stroke "#1E1E24" :stroke-width 2
             :style {:-fx-cursor "crosshair"}
             :on-mouse-pressed (fn [^MouseEvent e] (.consume e))
             :on-mouse-clicked (fn [^MouseEvent e]
                                 (.consume e)
                                 (when (= (.getButton e) MouseButton/PRIMARY)
                                   (handle-event {:event/type :click-output-port
                                                  :node-id (:id node)
                                                  :port-id p})))}))
        (:outputs node))}]}))

;; --- Top Level UI Shell ---

(defn root-view [{:keys [nodes edges connecting status]}]
  {:fx/type :stage
   :showing true
   :title "CloGraph - Visual Dataflow Editor"
   :width 960
   :height 620
   :scene
   {:fx/type :scene
    :root
    {:fx/type :border-pane
     :style {:-fx-background-color "#1E1E24"}

     :left
     {:fx/type :v-box
      :pref-width 200
      :spacing 10
      :style {:-fx-background-color "#21252B"
              :-fx-padding 14
              :-fx-border-color "#333842"
              :-fx-border-width "0 1 0 0"}
      :children
      [{:fx/type :label :text "PALETTE" :style {:-fx-text-fill "#5C6370" :-fx-font-weight "bold" :-fx-font-size 11}}
       {:fx/type :button :text "+ Number Value" :pref-width 170
        :on-action {:event/type :add-node :node-type :val}}
       {:fx/type :button :text "+ Function Node" :pref-width 170
        :on-action {:event/type :add-node :node-type :fn}}
       {:fx/type :button :text "+ Output Sink" :pref-width 170
        :on-action {:event/type :add-node :node-type :output}}

       {:fx/type :separator :style {:-fx-padding "8 0 8 0"}}

       {:fx/type :label :text "ACTIONS" :style {:-fx-text-fill "#5C6370" :-fx-font-weight "bold" :-fx-font-size 11}}
       {:fx/type :button
        :text "▶ RUN GRAPH"
        :pref-width 170
        :style {:-fx-background-color "#98C379" :-fx-text-fill "#1E1E24" :-fx-font-weight "bold" :-fx-padding 8}
        :on-action {:event/type :run-graph}}
       {:fx/type :button
        :text "Clear Wires"
        :pref-width 170
        :style {:-fx-background-color "#3E4451" :-fx-text-fill "#ABB2BF"}
        :on-action {:event/type :clear-wires}}

       {:fx/type :separator :style {:-fx-padding "8 0 8 0"}}

       {:fx/type :label :text "STATUS" :style {:-fx-text-fill "#5C6370" :-fx-font-weight "bold" :-fx-font-size 11}}
       {:fx/type :label
        :text (or status "Ready.")
        :wrap-text true
        :pref-width 170
        :style {:-fx-text-fill "#ABB2BF" :-fx-font-size 11}}]}

     :center
     {:fx/type :pane
      :on-mouse-clicked {:event/type :cancel-connecting}
      :children
      (concat
       (map (fn [edge]
              (render-wire (assoc edge :nodes nodes)))
            edges)
       (map (fn [node]
              (render-node {:node node :connecting connecting}))
            nodes))}}}})

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc (fn [state] (root-view state)))
   :opts {:fx.opt/map-event-handler handle-event}))

(defn -main [& _args]
  (fx/mount-renderer *state renderer))

(-main)