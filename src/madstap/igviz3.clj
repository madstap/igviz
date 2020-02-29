(ns madstap.igviz3
  (:require
   [clojure.java.io :as io]
   [tangle.core :as tangle]
   [integrant.core :as ig]
   [medley.core :as medley]
   [clojure.string :as str]
   madstap.igviz.spec
   [clojure.java.shell :as sh]
   [weavejester.dependency :as dep]
   [clojure.set :as set]))

(defn dfs [pred coll]
  (filter pred (tree-seq coll? seq coll)))

(defn required-namespaces [k]
  (->> (#'ig/key->namespaces k)
       (keep #'ig/try-require)
       (into (sorted-set))))

(defn config->nodes [config]
  (set (map (fn [[k conf]]
              (let [namee (pr-str k)]
                #:igviz.node{:key             k
                             :config          conf
                             :required        (required-namespaces k)
                             :refs            (set (dfs ig/reflike? conf))
                             :id              (#'ig/normalize-key k)
                             :name            namee
                             :igviz.dot/attrs {:label namee}}))
            config)))

(defn dependencies->edges [mm]
  (vec (for [[k vs] mm, v vs] [k v])))

(defn edge-refs [key->node [src dest]]
  (->> (:igviz.node/refs (key->node src))
       (filter #(ig/derived-from? dest (ig/ref-key %)))
       (into (sorted-set-by ig/ref-key))))

(defn ref-str [reflike]
  (str (cond (ig/ref? reflike)    "#ig/ref"
             (ig/refset? reflike) "#ig/refset")
       " "
       (pr-str (ig/ref-key reflike))))

(defn refs-str [refs]
  (if (= 1 (count refs))
    (ref-str (first refs))
    (str "#{" (->> refs (map ref-str) (str/join ", ")) "}")))

(defn config->graph [config]
  (let [edges*    (-> config
                      (ig/dependency-graph)
                      :dependencies
                      (dependencies->edges))
        nodes     (config->nodes config)
        key->node (medley/index-by :igviz.node/key nodes)
        edges
        (set (map (fn [[src dest :as edge]]
                    (let [refs                    (edge-refs key->node edge)
                          [src-id dest-id :as id] (mapv #'ig/normalize-key edge)]
                      #:igviz.edge{:src             src
                                   :src-id          src-id
                                   :dest            dest
                                   :dest-id         dest-id
                                   :edge            edge
                                   :id              id
                                   :refs            refs}))
                  edges*))]
    #:igviz{:nodes nodes, :edges edges}))

(defn create-img [dot file]
  (io/copy (tangle/dot->image dot "png")
           (io/file file)))

(defmulti select
  "Select a part of the graph"
  {:arglists '([op graph selector])}
  (fn [op _ _] op))

(defmulti select-transform
  "Used if a select needs to transform the graph as well.
  (For example selecting nodes that aren't in the graph.)
  Defaults to a noop."
  {:arglists '([op graph selected])}
  (fn [op _ _] op))

(defmethod select-transform :default [_ graph _] graph)

(defmulti transform
  "Transform a selected part of the graph"
  {:arglists '([op graph selected arg])}
  (fn [op _ _ _] op))

(defn derived-nodes-1 [nodes k]
  (->> nodes
       (filter #(ig/derived-from? (:igviz.node/id %) (#'ig/normalize-key k)))
       (map :igviz.node/key)))

(defn derived-nodes [nodes ks]
  (->> ks (mapcat (partial derived-nodes-1 nodes)) (set)))

(defn fully-connected-edges [nodes edges]
  (let [node-ks (set nodes)]
    (set (filter (fn [[src dest]]
                   (and (contains? node-ks src)
                        (contains? node-ks dest)))
                 edges))))

(defn graph->config [{:igviz/keys [nodes]}]
  (into {} (map (juxt :igviz.node/key :igviz.node/config)) nodes))

(defn dependencies-inclusive [{:igviz/keys [nodes] :as graph} ks include-refsets?]
  (let [derived (derived-nodes nodes ks)
        deps    (-> graph
                    graph->config
                    (ig/dependency-graph {:include-refsets? include-refsets?})
                    (dep/transitive-dependencies-set derived))]
    (into derived deps)))

;; TODO: Better names for :dependencies and :ks
(defmethod select :dependencies
  [_ {:igviz/keys [edges] :as graph} ks]
  (let [ks (if (set? ks) ks #{ks})
        node-deps (dependencies-inclusive graph ks true)]
    #:igviz.selected{:nodes node-deps
                     :edges (fully-connected-edges node-deps
                                                   (map :igviz.edge/edge edges))}))

(defmethod select :ks
  [_ {:igviz/keys [edges] :as graph} ks]
  (let [ks (if (set? ks) ks #{ks})
        node-deps (dependencies-inclusive graph ks false)]
    #:igviz.selected{:nodes node-deps
                     :edges (fully-connected-edges node-deps
                                                   (map :igviz.edge/edge edges))}))

(defmethod select :derived
  [_ {:igviz/keys [nodes edges]} ks]
  (let [ks         (if (coll? ks) (set ks) #{ks})
        derived-ns (derived-nodes nodes ks)
        derived-es (fully-connected-edges derived-ns (map :igviz.edge/edge edges))]
    #:igviz.selected{:nodes derived-ns
                     :edges derived-es}))

(defmethod select :derived-nodes
  [_ graph ks]
  (-> (select :derived graph ks) (dissoc :igviz.selected/edges)))

(defmethod select :derived-edges
  [_ graph ks]
  (-> (select :derived graph ks) (dissoc :igviz.selected/nodes)))

(defn filter-set [pred coll]
  (into #{} (filter pred) coll))

(defmethod transform :select
  [_ graph {:igviz.selected/keys [nodes edges]} _]
  (-> graph
      (update :igviz/nodes (partial filter-set #(contains? nodes (:igviz.node/key %))))
      (update :igviz/edges (partial filter-set #(contains? edges (:igviz.edge/edge %))))))

(def entity-kind->key
  {:igviz/nodes :igviz.node/key
   :igviz/edges :igviz.edge/edge})

(def selected-kind->entity-kind
  {:igviz.selected/nodes :igviz/nodes
   :igviz.selected/edges :igviz/edges})

(defn map-set [s f]
  (into #{} (map f) s))

(defn graph->selection [graph]
  (-> (reduce-kv (fn [g kind k]
                   (medley/update-existing g kind map-set k))
                 graph
                 entity-kind->key)
      (set/rename-keys (set/map-invert selected-kind->entity-kind))))

(comment

  (graph->selection #:igviz{:nodes #{#:igviz.node{:key ::foo}
                                     #:igviz.node{:key ::bar}}
                            :edges #{#:igviz.edge{:edge [::foo ::bar]}}})


  )


(defmethod select :all
  [_ graph _]
  (graph->selection graph))

(defmethod select :all-nodes
  [_ graph _]
  (-> (graph->selection graph) (select-keys [:igviz.selected/nodes])))

(defmethod select :all-edges
  [_ graph _]
  (-> (graph->selection graph) (select-keys [:igviz.selected/edges])))

(defn compose [& rules]
  (vec (mapcat seq rules)))

(defn- update-entities [graph kind ks f args]
  (let [k->entity (medley/index-by (entity-kind->key kind) (kind graph))]
    (set (vals (reduce #(apply medley/update-existing %1 %2 f args) k->entity ks)))))

(defn update-selected [graph selected f & args]
  (reduce-kv (fn [g kind ks]
               (let [ent-kind (selected-kind->entity-kind kind)]
                 (assoc g ent-kind (update-entities g ent-kind ks f args))))
             graph
             (select-keys selected (keys selected-kind->entity-kind))))

(defmethod transform :merge-attrs
  [_ graph selected attrs]
  (update-selected graph selected update :igviz.dot/attrs merge attrs))

(defn expand-rules [rules]
  (for [[selector ops]            rules
        [selector-arg transforms] ops
        [transform transform-arg] (if (keyword? transforms)
                                    {transforms nil}
                                    transforms)]
    [selector selector-arg transform transform-arg]))

(defn apply-rules [graph rules]
  (reduce (fn [g1 [sel sel-arg trans trans-arg]]
            (let [selected (select sel g1 sel-arg)
                  g2       (select-transform sel g1 selected)]
              (transform trans g2 selected trans-arg)))
          graph
          (expand-rules rules)))

(defn dot-edges [edges]
  (map (fn [{:igviz.edge/keys [src-id dest-id]
             :igviz.dot/keys [attrs]}]
         [src-id dest-id attrs])
       edges))

(defn graph->dot
  ([graph]       (graph->dot graph {}    {}))
  ([graph rules] (graph->dot graph rules {}))
  ([graph rules {:keys [node]
                 :or   {node {:shape :oval}}}]
   (let [{:igviz/keys [nodes edges]} (apply-rules graph rules)]
     (tangle/graph->dot
      nodes
      (dot-edges edges)
      {:node             node
       :directed?        true
       :node->id         :igviz.node/id
       :node->descriptor :igviz.dot/attrs
       :edge->descriptor (fn [_ _ attrs] attrs)}))))

(defn xdg-open [path]
  (future (sh/sh "xdg-open" path)))

(defmethod transform :label-refs
  [_ graph selected _]
  (update-selected graph selected
                   (fn [entity]
                     (update-in entity [:igviz.dot/attrs :label]
                                (fn [label]
                                  (str label
                                       (when-not (str/blank? label) \newline)
                                       (refs-str (or (:igviz.edge/refs entity)
                                                     (:igviz.node/refs entity)))))))))

(def label-edges
  {:all-edges {nil {:label-refs nil}}})

(defmethod select :removed*
  [_ graph old-graph]
  (let [new (graph->selection graph)
        old (graph->selection old-graph)]
    (merge-with set/difference new old)))

(defmethod select :removed
  [_ graph old-config]
  (select :removed* graph (config->graph old-config)))

(defmethod select :added*
  [_ graph old-graph]
  (let [new (graph->selection graph)
        old (graph->selection old-graph)]
    (-> (merge-with set/difference old new)
        (assoc ::old-graph old-graph))))

(defmethod select :added
  [_ graph old-config]
  (select :added* graph (config->graph old-config)))

(defn- into-graph [g1 g2]
  (-> g1
      (update :igviz/nodes #(->> (concat % (:igviz/nodes g2))
                                 (medley/distinct-by :igviz.node/key)
                                 set))
      (update :igviz/edges #(->> (concat % (:igviz/edges g2))
                                 (medley/distinct-by :igviz.edge/edge)
                                 set))))

(defmethod select-transform :added*
  [_ graph {::keys [old-graph] :as x}]
  (into-graph graph old-graph))

(defmethod select-transform :added
  [_ graph {::keys [old-config] :as selected}]
  (let [g (config->graph old-config)]
    (select-transform :added* graph (assoc selected ::old-graph g))))

(defn diff* [old-graph]
  {:added*   {old-graph {:merge-attrs {:color :green}}}
   :removed* {old-graph {:merge-attrs {:color :red}}}})

(defn diff [old-config]
  (diff* (config->graph old-config)))

(comment
  (require '[madstap.comfy :refer [defs]]
           '[kafka :refer [config sconf sconf2]])

  (def rules
    (compose
     label-edges
     {:derived {:kafka/topic {:merge-attrs     {;; :color  :green
                                                :shape  :box
                                                :height 0.5
                                                :width  4}
                              #_#_:show-config [:topic-name]
                              }
                :kafka/db    {:merge-attrs {:shape :cylinder}
                              ;; :show-config [:db-name]
                              }}
      :ks      {:kafka/consumer1 {:merge-attrs {:color :red}}
                ;; :kafka/error-component :select
                }}
     (diff sconf2)))

  (def g (config->graph config))

  (transform-graph g rules)

  (-> sconf config->graph (graph->dot rules) (create-img "sys.png"))

  (xdg-open "sys.png")

  )
