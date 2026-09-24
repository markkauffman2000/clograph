(ns clograph.core
  (:require [cljfx.api :as fx]
            [clograph.compiler :as comp])
  (:gen-class))

;; --- State & Initial Top-to-Bottom Layout ---

(def node-width 140.0)
(def node-height 75.0)

(def initial-state
  {:nodes
   [;; Top row: Input values
    {:id :val-1 :type :val    :value 15  :x 160.0 :y 50.0  :outputs [:out]}
    {:id :val-2 :type :val    :value 27  :x 380.0 :y 50.0  :outputs [:out]}
    ;; Middle row: + Function
    {:id :fn-1  :type :fn     :op '+     :x 270.0 :y 190.0 :inputs [:in-a :in-b] :outputs [:out]}
    ;; Bottom row: Output Sink
    {:id :out-1 :type :output :result nil :x 270.0 :y 340.0 :inputs [:in]}]

   ;; Pre-wired edges: val-1 & val-2 -> fn-1 -> out-1
   :edges
   [{:from-node :val-1 :from-port :out :to-node :fn-1 :to-port :in-a}
    {:from-node :val-2 :from-port :out :to-node :fn-1 :to-port :in-b}
    {:from-node :fn-1  :from-port :out :to-node :out-1 :to-port :in}]

   :connecting nil   ;; {:from-node ... :from-port ...}
   :drag-state nil   ;; {:node-id ... :mouse-x ... :mouse-y ... :node-x ... :node-y ...}
   :status "Ready. Click RUN GRAPH or drag nodes."})

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

;; --- Central Event Handler ---

(defn handle-event [{:keys [event/type] :as event}]
  (case type
    :add-node
    (let [ntype (:node-type event)
          id (keyword (str (name ntype) "-" (rand-int 1000)))
          new-node (case ntype
                     :val    {:id id :type :val :value 10 :x 100.0 :y 100.0 :outputs [:out]}
                     :fn     {:id id :type :fn  :op '+    :x 100.0 :y 100.0 :inputs [:in-a :in-b] :outputs [:out]}
                     :output {:id id :type :output :result nil :x 100.0 :y 100.0 :inputs [:in]})]
      (swap! *state (fn [s]
                      (-> s
                          (update :nodes conj new-node)
                          (assoc :status (str "Added " (name ntype) " node."))))))

    :update-val
    (let [val (or (try (Long/parseLong (:val event)) (catch Exception _ nil)) 0)]
      (swap! *state update :nodes
             (fn [nodes]
               (mapv #(if (= (:id %) (:node-id event)) (assoc % :value val) %) nodes))))

    :click-output-port
    (swap! *state assoc
           :connecting {:from-node (:node-id event) :from-port (:port-id event)}
           :status (str "Wiring from " (name (:node-id event)) ". Click a green input port."))

    :click-input-port
    (swap! *state
           (fn [{:keys [connecting edges] :as state}]
             (if connecting
               (let [new-edge {:from-node (:from-node connecting)
                               :from-port (:from-port connecting)
                               :to-node   (:node-id event)
                               :to-port   (:port-id event)}
                     ;; Remove any existing edge connected to this specific input
                     clean-edges (vec (remove #(and (= (:to-node %) (:node-id event))
                                                    (= (:to-port %) (:port-id event)))
                                              edges))]
                 (-> state
                     (assoc :connecting nil)
                     (assoc :edges (conj clean-edges new-edge))
                     (assoc :status "Wire connected!")))
               state)))

    :cancel-connecting
    (swap! *state assoc :connecting nil :status "Wiring cancelled.")

    :clear-wires
    (swap! *state assoc :edges [] :connecting nil :status "Cleared all wires.")

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
      (when node
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
       ;; Downward control points for classic Prograph flow
       :control-x1 x1 :control-y1 (+ y1 50.0)
       :control-x2 x2 :control-y2 (- y2 50.0)
       :stroke "#61AFEF"
       :stroke-width 3.0
       :fill :transparent ;; can't be nil!!
       ;; Critical: curves must be mouse-transparent so they don't block clicks beneath them
       :mouse-transparent true}
      {:fx/type :group})))

(defn render-node [{:keys [node connecting]}]
  (let [is-output (= :output (:type node))]
    {:fx/type :group
     :layout-x (:x node)
     :layout-y (:y node)
     :children
     [;; 1. The Card Body (handles dragging)
      {:fx/type :v-box
       :pref-width node-width
       :pref-height node-height
       :alignment :center
       :spacing 4
       :on-mouse-pressed {:event/type :start-drag :node-id (:id node)}
       :on-mouse-dragged {:event/type :drag-node}
       :on-mouse-released {:event/type :stop-drag}
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
         [{:fx/type :label
           :text "FUNCTION"
           :mouse-transparent true
           :style {:-fx-text-fill "#5C6370" :-fx-font-size 9 :-fx-font-weight "bold"}}
          {:fx/type :label
           :text (str (:op node))
           :mouse-transparent true
           :style {:-fx-text-fill "#61AFEF"
                   :-fx-font-size 22
                   :-fx-font-weight "bold"}}]

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
             ;; Consume mouse pressed so it doesn't drag the node
             :on-mouse-pressed (fn [^javafx.scene.input.MouseEvent e] (.consume e))
             :on-mouse-clicked (fn [^javafx.scene.input.MouseEvent e]
                                 (.consume e)
                                 (handle-event {:event/type :click-input-port
                                                :node-id (:id node)
                                                :port-id p}))}))
        (:inputs node))}

      ;; 3. Bottom Output Ports (Blue / Gold highlight when active)
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
             ;; Consume mouse pressed so it doesn't drag the node
             :on-mouse-pressed (fn [^javafx.scene.input.MouseEvent e] (.consume e))
             :on-mouse-clicked (fn [^javafx.scene.input.MouseEvent e]
                                 (.consume e)
                                 (handle-event {:event/type :click-output-port
                                                :node-id (:id node)
                                                :port-id p}))}))
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

     ;; Left Palette
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
       {:fx/type :button :text "+ Function (+)" :pref-width 170
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

     ;; Right Canvas
     :center
     {:fx/type :pane
      :on-mouse-clicked {:event/type :cancel-connecting}
      :children
      (concat
       ;; Layer 1: Wires (underneath)
       (map (fn [edge]
              (render-wire (assoc edge :nodes nodes)))
            edges)
       ;; Layer 2: Nodes (on top)
       (map (fn [node]
              (render-node {:node node :connecting connecting}))
            nodes))}}}})

;; Renderer configured with the map event handler enabled
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc (fn [state] (root-view state)))
   :opts {:fx.opt/map-event-handler handle-event}))

(defn -main [& _args]
  (fx/mount-renderer *state renderer))

(-main)