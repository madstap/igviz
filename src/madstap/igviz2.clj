(ns madstap.igviz2
  (:require
   [clojure.java.shell :as sh]
   [clojure.pprint :refer [pprint]]
   [integrant.core :as ig]
   [integrant.repl :as ig.repl]
   [tangle.core :as tangle]
   [clojure.java.browse :refer [browse-url]]
   [clojure.java.io :as io]
   [weavejester.dependency :as dep]
   [medley.core :as medley]
   [madstap.comfy :as comfy :refer [defs]]
   [clojure.java.io :as io]
   ;; [ubergraph.core :as uber]
   [clojure.string :as str]
   [meta-merge.core :refer [meta-merge]]
   [clojure.alpha.spec :as s]))

(defn pp-str [x]
  (with-out-str
    (pprint (if (map? x) (into (sorted-map) x) x))))

(defn create-img [dot file]
  (io/copy (tangle/dot->image dot "png")
           (io/file file)))

(defn dependencies->edges
  ([mm]
   (dependencies->edges mm identity))
  ([mm f]
   (vec (for [[k vs] mm, v vs]
          [(f k) (f v)]))))

(defn method-class [k]
  (some-> (get-method ig/init-key k) (bean) :class))

(defn config->edges [config]
  (-> config
      (ig/dependency-graph)
      :dependencies
      (dependencies->edges #'ig/normalize-key)))

(defn postwalk-into [to xf from]
  (comfy/postwalk-transduce xf conj to from))

(defn required-namespaces [k]
  (->> (#'ig/key->namespaces k)
       (keep #'ig/try-require)
       (into (sorted-set))))

(defn config->nodes [config]
  (map (fn [[k conf]]
         #::{:key        k
             :config     conf
             :required   (required-namespaces k)
             :references (postwalk-into #{}
                                        (filter ig/reflike?)
                                        conf)
             :id         (#'ig/normalize-key k)
             :name       (pr-str k)})
       config))

(defn derivee?
  [key candidate]
  (ig/derived-from? candidate key))

(defn ancestors-inclusive [k]
  (conj (ancestors k) k))

(defn derivee-set [m k]
  (->> m
       (medley/filter-keys #(derivee? % k))
       (vals)
       (mapcat seq)
       (set)))

(defn merge-derivees [m k]
  (->> m
       (medley/filter-keys #(derivee? % k))
       (vals)
       (apply (partial merge-with (fn [x y]
                                    ;; TODO: This should probably try to use the
                                    ;;       most specific key and only print a
                                    ;;       warning if there are two or more
                                    ;;       keys of the same specificity.
                                    (println "WARNING: Same key specified in two"
                                             "different attribute maps."
                                             "Which one of these overwrites the"
                                             "other is non-deterministic."
                                             (pr-str x) "was overwritten by "
                                             (pr-str y) ".")
                                    y)))))

(defn eog [path]
  (future (sh/sh "eog" path)))

;; adding generic way to open files on x to visualize
(defn xdg-open-file [path]
  (future (sh/sh "xdg-open" path)))

(defn keyset->component-keys [config ks]
  (->> ks
     (mapcat (partial ig/find-derived config))
     (map first)
     (set)))

(defn transitive-deps-inclusive [config ks {:keys [include-refsets?]
                                            :or   {include-refsets? false}}]
  (let [components (keyset->component-keys config ks)]
    (-> (ig/dependency-graph config {:include-refsets? include-refsets?})
        (dep/transitive-dependencies-set components)
        (into components))))

(defn select-components [config ks {:keys [include-refsets?]
                                    :or   {include-refsets? false}}]
  (let [deps (transitive-deps-inclusive config ks {:include-refsets? include-refsets?})]
    (-> config (select-keys deps))))

(defn edge-refs [id->node src dest]
  (->> (::references (id->node src))
       (filter #(ig/derived-from? dest (ig/ref-key %)))))

(defn ref-str [reflike]
  (str (cond (ig/ref? reflike)    "#ig/ref"
             (ig/refset? reflike) "#ig/refset")
       " "
       (pr-str (ig/ref-key reflike))))

(defn refs-str [refs]
  (condp #(<= %1 (count %2)) refs
    2 (str "#{" (->> refs (map ref-str) (str/join ", ")) "}")
    1 (ref-str (first refs))))

(defn dot [config {:keys [hierarchy selected-components
                          derived-attrs derived-show-config
                          node label-edges?
                          include-refsets?]
                   :or   {hierarchy        @#'clojure.core/global-hierarchy
                          node             {:shape :oval}
                          label-edges?     true
                          include-refsets? false}}]
  (let [conf     (cond-> config
                   (seq selected-components)
                   (select-components selected-components {:include-refsets?
                                                           include-refsets?}))
        nodes    (config->nodes conf)
        id->node (medley/index-by ::id nodes)]
    (tangle/graph->dot
     nodes
     (config->edges conf)
     {:node             node
      :directed?        true
      :node->id         ::id
      :node->descriptor (fn [{k        ::key
                              n        ::name
                              conf     ::config
                              required ::required
                              :as      x}]
                          (merge {:label (str n "\n"
                                              (when required (str "ns: " required)) "\n"
                                              (when (map? conf)
                                                (-> conf
                                                    (select-keys (derivee-set derived-show-config k))
                                                    (not-empty))))}
                                 (merge-derivees derived-attrs k)))
      :edge->descriptor (fn [src dest _]
                          (cond-> {}
                            label-edges? (assoc :label (refs-str (edge-refs id->node src dest)))))})))


(comment

  (meta-merge {:bar "green"}
              {:bar "blue"
               :baz 1233})

  (require '[kafka :refer [config]])

  (methods ig/init-key)

  (defmethod ig/init-key :kafka/consumer1 [_ _] {})

  :op
  #{:select
    ;; Selects nodes to be included in the graph
    :merge-attrs
    ;; merge graphviz node attrs using meta-merge
    :show-config
    ;; Takes a set of keys to be selected using select-keys from the value
    ;; (presumably a map) of the components in the config.
    }

  ;; Should there be non-inclusive versions of :dependencies and :dependents?
  :selector
  #{:dependencies
    ;; Selects the dependencies of components derived from a set of keys
    :dependents
    ;; Selects the dependents of components derived from a set of keys
    :derived
    ;; Given a set, selects the components derived from any of the keys (OR).
    ;; Given a vector, selects the components derived from all of the keys (AND).
    }



  (defn record->tx [{:keys [_ value offset partition topic-name timestamp]}]
    (let [{:pgo.tx-paygo-web/keys [id] :as parsed}
          (medley/remove-vals nil? (parse-tx-web value))
          ts-inst (java.util.Date. timestamp)
          kafka-metadata #:pgo.kafka{:offset     offset
                                     :partition  partition
                                     :topic-name topic-name
                                     :timestamp  ts-inst}]
      {:pgo.tx-paygo-web/data-source kafka-metadata
       :pgo.tx-paygo-web/id id
       :pgo.tx-unified/id            (pg-web-id/pg-web->uuid parsed)}))

  (meta-merge #{1 2} [4])


  {:rules {:ks      {:kafka/server {:select nil}}
           :derived {:kafka/topic {:merge-attrs {:color  :red
                                                 :shape  :box
                                                 :height 0.5
                                                 :width  4}
                                   :show-config #{:topic-name}}
                     :kafka/db    {:merge-attrs {:shape :cylinder}
                                   :show-config #{:db-name}}}}}

  {:rules [[:derived [[:kafka/db    [[:merge-attrs {:shape :cylinder}]
                                     [:show-config #{:db-name}]]]]]
           [:ks      [[:kafka/server [[:select nil]]]]]
           [:derived [[:kafka/topic [[:merge-attrs {:color  :red
                                                    :shape  :box
                                                    :height 0.5
                                                    :width  4}]
                                     [:show-config #{:topic-name}]]]]]]}




  {:rules {[:ks :kafka/server]     {:select nil}
           [:derived :kafka/topic] {:merge-attrs {:color  :red
                                                  :shape  :box
                                                  :height 0.5
                                                  :width  4}
                                    :show-config #{:topic-name}}
           [:derived :kafka/db]    {:merge-attrs {:shape :cylinder}}}}





  [[:select :dependencies]
   [:select :dependents]
   [:merge-attrs :derived]
   [:show-config :derived]]

  ;; Select with a map means OR, and a vector means AND
  {:dependencies {:select #{:kafka/server}}
   :dependents {:merge-attrs #{}}
   :derived      {:merge-attrs {:kafka/topic {:color  :red
                                              :shape  :box
                                              :height 0.5
                                              :width  4}}}}

  [:dependencies :select]
  [:dependents :select]
  [:derived :merge-attrs]
  [:derived :show-config]
  :label-edges?

  [ {:dependencies #{:kafka/server :duct/daemon}}]

  {:select {:dependencies #{:kafka/server :duct/daemon}}
   :merge-attrs {:derived {:kafka/topic {:color  :red
                                         :shape  :box
                                         :height 0.5
                                         :width  4}
                           :kafka/db    {:shape :cylinder}}}
   ;; :include-refsets?    true
   :derived-attrs       {:kafka/topic {:color  :red
                                       :shape  :box
                                       :height 0.5
                                       :width  4}
                         :kafka/db    {:shape :cylinder}}
   :derived-show-config {:kafka/topic #{:topic-name}}
   ;; :label-edges?        false
   }

  (-> config
      (dot {:selected-components #{:kafka/server :duct/daemon}
            ;; :include-refsets?    true
            :derived-attrs       {:kafka/topic {:color  :red
                                                :shape  :box
                                                :height 0.5
                                                :width  4}
                                  :kafka/db    {:shape :cylinder}}
            :derived-show-config {:kafka/topic #{:topic-name}}
            ;; :label-edges?        false
            })
      (create-img "kafka-sys4.png"))

  (ig/init config #_[:kafka/server])

  (get-method ig/init-key :kafka/consumer)

  (ancestors-inclusive (#'ig/normalize-key [:kafka/consumer1 :kafka/consumer]))

  (remove-method ig/init-key :kafka/consumer1)

  (def *s
    (eog "kafka-sys4.png"))

  (select-components config #{:kafka/server} {:include-refsets? true})

  (select-components config #{:kafka/server} {:include-refsets? false})

  (derive :kafka/foo-topic :kafka/topic)

  (-> config
      ;; (select-components #{:kafka/error-component})
      ;; (select-components #{:kafka/server})
      (select-components #{[:kafka/consumer1 :kafka/consumer]})
      (system-dot {:kafka/topic {:color  :red
                                 :shape  :box
                                 :height 0.5
                                 :width  4}
                   :kafka/db    {:shape :cylinder}})
      (create-img "examples/hello-s.png"))

  (get-method ig/init-key :kafka/foo-topic)

  (def *s
    (eog "examples/hello-s.png"))

  @*s)



(comment

  (println "asd")

  (defs {ps :parents
         as :ancestors}
    @#'clojure.core/global-hierarchy)

  ;; Then a mapping between participant keys and actual components.

  (config->nodes config)

  (-> (tangle/graph->dot
       {}
       (dependencies->edges ps)
       {:node             {:shape :oval}
        :directed?        true
        :node->id         pr-str
        :node->descriptor (fn [n] {:label (pr-str n)})})
      (create-img "examples/hello.png"))

  (eog "examples/hello.png")

  (defs {:keys [dependencies dependents]}
    (ig/dependency-graph config))

  (-> (tangle/graph->dot
       {}
       (dependencies->edges dependencies #'ig/normalize-key)
       {:node             {:shape :oval}
        :directed?        true
        :node->id         pr-str
        :node->descriptor (fn [n] {:label (pr-str n)})})
      (create-img "examples/hello.png"))

  )


(comment

  (def html-node {:id "html" :color "blue" :label [:TABLE {:BORDER 0} [:TR [:TD "hic"] [:TD {:BORDER 1} "cup"]]]})

  (def nodes [:a :b :c :d html-node])

  (def edges [[:a :b] [:a :c] [:c :d] [:a :c {:label "another" :style :dashed}] [:a :html]])

  (def dot
    (tangle/graph->dot
     nodes
     edges
     {:node {:shape :oval}
      :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
      :node->descriptor (fn [n] (when-not (keyword? n) n))}))



  )
