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
  (mapv (fn [[k conf]]
          (let [namee (pr-str k)]
            #:igviz.node{:key             k
                         :config          conf
                         :required        (required-namespaces k)
                         :refs            (set (dfs ig/reflike? conf))
                         :id              (#'ig/normalize-key k)
                         :name            namee
                         :igviz.dot/attrs {:label namee}}))
        config))

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
        (mapv (fn [[src dest :as edge]]
                (let [refs                    (edge-refs key->node edge)
                      [src-id dest-id :as id] (mapv #'ig/normalize-key edge)]
                  #:igviz.edge{:src             src
                               :src-id          src-id
                               :dest            dest
                               :dest-id         dest-id
                               :edge            edge
                               :id              id
                               :refs            refs
                               :igviz.dot/attrs {:label (refs-str refs)}}))
              edges*)]
    #:igviz{:nodes nodes, :edges edges}))

(defn create-img [dot file]
  (io/copy (tangle/dot->image dot "png")
           (io/file file)))

(defmulti select
  "Select a part of the graph"
  {:arglists '([op graph selector])}
  (fn [op _ _] op))

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


(defn- update-entities [graph kind ks f args]
  (let [k->entity (medley/index-by (entity-kind->key kind) (kind graph))]
    (set (vals (reduce #(apply medley/update-existing %1 %2 f args) k->entity ks)))))

(defn update-selected [graph selected f & args]
  (reduce-kv (fn [g kind ks]
               (let [ent-kind (selected-kind->entity-kind kind)]
                 (assoc g ent-kind (update-entities g ent-kind ks f args))))
             graph
             selected))

(defmethod transform :merge-attrs
  [_ graph selected attrs]
  (update-selected graph selected update :igviz.dot/attrs merge attrs))

(defn map-like? [x]
  (or (map? x)
      (and (vector? x)
           (every? (every-pred vector? #(= 2 (count %))) x))))

(defn expand-rules [rules]
  (for [[selector ops]            rules
        [selector-arg transforms] ops
        [transform transform-arg] (if (map-like? transforms) transforms {transforms nil})]
    [selector selector-arg transform transform-arg]))

(defn transform-graph [graph rules]
  (reduce (fn [g [sel sel-arg trans trans-arg]]
            (let [selected (select sel g sel-arg)]
              (transform trans g selected trans-arg)))
          graph
          (expand-rules rules)))

(defn graph->dot
  [graph {:keys [rules node label-edges?]
          :or   {node         {:shape :oval}
                 label-edges? true}}]
  (let [{:igviz/keys [nodes edges]} (transform-graph graph rules)
        id->edge                    (medley/index-by :igviz.edge/id edges)]
    (tangle/graph->dot
     nodes
     (map :igviz.edge/id edges)
     {:node             node
      :directed?        true
      :node->id         :igviz.node/id
      :node->descriptor :igviz.dot/attrs
      :edge->descriptor (fn [src dest _]
                          (-> (id->edge [src dest])
                              :igviz.dot/attrs
                              (cond-> (not label-edges?) (dissoc :label))))})))

(defn xdg-open [path]
  (future (sh/sh "xdg-open" path)))

(comment
  (require '[madstap.comfy :refer [defs]]
           '[kafka :refer [config]])

  (defs {:keys [rules] :as opts}
    {:label-edges? true
     :rules        [[:derived {:kafka/topic {:merge-attrs     {:color  :green
                                                               :shape  :box
                                                               :height 0.5
                                                               :width  4}
                                             #_#_:show-config [:topic-name]
                                             }
                               :kafka/db    {:merge-attrs {:shape :cylinder}
                                             ;; :show-config [:db-name]
                                             }}]
                    [:ks {:kafka/consumer1 {:merge-attrs {:color :red}}}]
                    [:ks {:kafka/error-component :select}]]})


  (def g (config->graph config))

  (transform-graph g rules)

  (-> config config->graph (graph->dot opts) (create-img "sys.png"))

  (xdg-open "sys.png")

  )
