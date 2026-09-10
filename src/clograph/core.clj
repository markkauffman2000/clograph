
;; This defines the cljfx UI, port hitboxes, wire curves, dragging, and execution triggers.

(ns clograph.core
  (:require [cljfx.api :as fx]
            [clograph.compiler :as comp])
  (:gen-class))

;; --- State & Defaults ---

(def node-width 130.0)
(def node-height 70.0)

(def initial-state
  {:nodes [{:id :val-1 :type :val    :value 15 :x 40.0  :y 80.0  :outputs [:out]}
           {:id :val-2 :type :val    :value 27 :x 40.0  :y 200.0 :outputs [:out]}
           {:id :fn-1  :type :fn     :op '+    :x 240.0 :y 140.0 :inputs [:in-a :in-b] :outputs [:out]}
           {:id :out-1 :type :output :result nil :x 440.0 :y 140.0 :inputs [:in]}]
   :edges []
   :connecting nil ;; {:from-node ... :from-port ...}
   :drag-state nil})

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

;; --- UI Event Handlers ---

(defn handle-event [{:keys [event/type] :as event}]
  (case type
    :add-node
    (let [ntype (:node-type event)
          id (keyword (str (name ntype) "-" (rand-int 1000)))
          new-node (case ntype
                     :val    {:id id :type :val :value 10 :x 100.0 :y 100.0 :outputs [:out]}
                     :fn     {:id id :type :fn :op '+ :x 100.0 :y 100.0 :inputs [:in-a :in-b] :outputs [:out]}
                     :output {:id id :type :output :result nil :x 100.0 :y 100.0 :inputs [:in]})]
      (swap! *state update :nodes conj new-node))

    :update-val
    (let [val (or (try (Long/parseLong (:val event)) (catch Exception _ nil)) 0)]
      (swap! *state update :nodes
             (fn [nodes]
               (mapv #(if (= (:id %) (:node-id event)) (assoc % :value val) %) nodes))))

    :click-output-port
    (swap! *state assoc :connecting {:from-node (:node-id event)
                                     :from-port (:port-id event)})

    :click-input-port
    (swap! *state (fn [{:keys [connecting edges] :as state}]
                    (if connecting
                      (let [new-edge {:from-node (:from-node connecting)
                                      :from-port (:from-port connecting)
                                      :to-node   (:node-id event)
                                      :to-port   (:port-id event)}]
                        (-> state
                            (assoc :connecting nil)
                            (update :edges conj new-edge)))
                      state)))

    :cancel-connecting
    (swap! *state assoc :connecting nil)

    :run-graph
    (try
      (let [results (comp/run-graph @*state)]
        (swap! *state update :nodes
               (fn [nodes]
                 (mapv (fn [n]
                         (if (= (:type n) :output)
                           (assoc n :result (get results (:id n) "nil"))
                           n))
                       nodes))))
      (catch Exception ex
        (println "[Execution Error]:" (.getMessage ex))))

    :start-drag
    (let [fx-e (:fx/event event)]
      (swap! *state assoc :drag-state {:node-id (:node-id event)
                                       :start-x (.getSceneX fx-e)
                                       :start-y (.getSceneY fx-e)}))

    :drag-node
    (let [{:keys [node-id start-x start-y]} (:drag-state @*state)
          fx-e (:fx/event event)]
      (when node-id
        (let [dx (- (.getSceneX fx-e) start-x)
              dy (- (.getSceneY fx-e) start-y)]
          (swap! *state (fn [s]
                          (-> s
                              (update :nodes (fn [nodes]
                                               (mapv (fn [n]
                                                       (if (= (:id n) node-id)
                                                         (-> n
                                                             (update :x + dx)
                                                             (update :y + dy))
                                                         n))
                                                     nodes)))
                              (assoc :drag-state {:node-id node-id
                                                  :start-x (.getSceneX fx-e)
                                                  :start-y (.getSceneY fx-e)})))))))

    :stop-drag
    (swap! *state assoc :drag-state nil)

    nil))

;; --- Visual Components ---

(defn render-wire [{:keys [from-node from-port to-node to-port nodes]}]
  (let [[x1 y1] (port-coord nodes from-node from-port :out)
        [x2 y2] (port-coord nodes to-node to-port :in)]
    (if (and x1 y1 x2 y2)
      {:fx/type :cubic-curve
       :start-x x1 :start-y y1
       :end-x x2   :end-y y2
       :control-x1 x1 :control-y1 (+ y1 45.0)
       :control-x2 x2 :control-y2 (- y2 45.0)
       :stroke "#4A90E2"
       :stroke-width 2.5
       :fill nil}
      {:fx/type :group})))

(defn render-node [{:keys [node connecting]}]
  (let [is-output (= :output (:type node))]
    {:fx/type :group
     :layout-x (:x node)
     :layout-y (:y node)
     :children
     [;; Main Card Box
      {:fx/type :v-box
       :pref-width node-width
       :pref-height node-height
       :on-mouse-pressed {:event/type :start-drag :node-id (:id node)}
       :on-mouse-dragged {:event/type :drag-node}
       :on-mouse-released {:event/type :stop-drag}
       :style {:-fx-background-color "#2D3139"
               :-fx-border-color (if is-output "#50E3C2" "#3E4451")
               :-fx-border-width 2
               :-fx-border-radius 6
               :-fx-background-radius 6
               :-fx-padding 8
               :-fx-alignment "center"}}

      ;; Content inside node
      (case (:type node)
        :val
        {:fx/type :v-box
         :layout-x 15.0 :layout-y 15.0
         :children [{:fx/type :label :text "Value" :style {:-fx-text-fill "#ABB2BF" :-fx-font-size 10}}
                    {:fx/type :text-field
                     :pref-width 90.0
                     :text (str (:value node))
                     :on-text-changed (fn [txt]
                                        (handle-event {:event/type :update-val
                                                       :node-id (:id node)
                                                       :val txt}))}]}

        :fn
        {:fx/type :label
         :layout-x 45.0 :layout-y 18.0
         :text (str (:op node))
         :style {:-fx-text-fill "#E5C07B"
                 :-fx-font-size 22
                 :-fx-font-weight "bold"}}

        :output
        {:fx/type :v-box
         :layout-x 15.0 :layout-y 12.0
         :children [{:fx/type :label :text "Output Sink" :style {:-fx-text-fill "#50E3C2" :-fx-font-size 10}}
                    {:fx/type :label
                     :text (str (or (:result node) "---"))
                     :style {:-fx-text-fill "#FFFFFF"
                             :-fx-font-size 16
                             :-fx-font-weight "bold"}}]})

      ;; Top Input Ports (Clickable)
      {:fx/type :group
       :children
       (map-indexed
        (fn [idx p]
          (let [spacing (/ node-width (inc (count (:inputs node))))
                px (* (inc idx) spacing)]
            {:fx/type :circle
             :center-x px :center-y 0.0 :radius 6.0
             :fill (if connecting "#E06C75" "#98C379")
             :stroke "#1E1E1E" :stroke-width 2
             :on-mouse-clicked {:event/type :click-input-port :node-id (:id node) :port-id p}}))
        (:inputs node))}

      ;; Bottom Output Ports (Clickable)
      {:fx/type :group
       :children
       (map-indexed
        (fn [idx p]
          (let [spacing (/ node-width (inc (count (:outputs node))))
                px (* (inc idx) spacing)]
            {:fx/type :circle
             :center-x px :center-y node-height :radius 6.0
             :fill "#61AFEF"
             :stroke "#1E1E1E" :stroke-width 2
             :on-mouse-clicked {:event/type :click-output-port :node-id (:id node) :port-id p}}))
        (:outputs node))}]}))

;; --- Top Level UI Shell ---

(defn root-view [{:keys [nodes edges connecting]}]
  {:fx/type :stage
   :showing true
   :title "CloGraph - Visual Dataflow Editor"
   :width 950
   :height 600
   :scene
   {:fx/type :scene
    :root
    {:fx/type :border-pane
     :style {:-fx-background-color "#1E1E24"}
     :left
     {:fx/type :v-box
      :pref-width 180
      :spacing 12
      :style {:-fx-background-color "#25252D"
              :-fx-padding 14
              :-fx-border-color "#33333D"
              :-fx-border-width "0 1 0 0"}
      :children
      [{:fx/type :label :text "PALETTE" :style {:-fx-text-fill "#5C6370" :-fx-font-weight "bold"}}
       {:fx/type :button :text "+ Number Value" :pref-width 150
        :on-action {:event/type :add-node :node-type :val}}
       {:fx/type :button :text "+ Function (+)" :pref-width 150
        :on-action {:event/type :add-node :node-type :fn}}
       {:fx/type :button :text "+ Output Sink" :pref-width 150
        :on-action {:event/type :add-node :node-type :output}}
       {:fx/type :separator}
       {:fx/type :label :text "ACTIONS" :style {:-fx-text-fill "#5C6370" :-fx-font-weight "bold"}}
       {:fx/type :button
        :text "▶ RUN GRAPH"
        :pref-width 150
        :style {:-fx-background-color "#98C379" :-fx-text-fill "#1E1E24" :-fx-font-weight "bold"}
        :on-action {:event/type :run-graph}}]}

     :center
     {:fx/type :pane
      :on-mouse-clicked {:event/type :cancel-connecting}
      :children
      (concat
       ;; 1. Render Wires (background)
       (map (fn [edge]
              (render-wire (assoc edge :nodes nodes)))
            edges)
       ;; 2. Render Nodes (foreground)
       (map (fn [node]
              (render-node {:node node :connecting connecting}))
            nodes))}}}})

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc (fn [state] (root-view state)))
   :opts {:fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                       (fx/fn->lifecycle %))}))

(defn -main [& _args]
  (fx/mount-renderer *state renderer)
  (add-watch *state :event-watch (fn [_ _ _ _] (renderer))))
