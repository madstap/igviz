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
   [clojure.java.io :as io]))

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
          [(f k) (f v) {:label "foo"}]))))

(defn method-class [k]
  (some-> (get-method ig/init-key k) (bean) :class))

(defn config->edges [config]
  (-> config (ig/dependency-graph) :dependencies (dependencies->edges #'ig/normalize-key)))

(defn postwalk-into [to xf from]
  (comfy/postwalk-transduce xf conj to from))

(defn config->nodes [config]
  (map (fn [[k conf]]
         #::{:key     k
             :config  conf
             :refs    (postwalk-into #{} (comp (filter ig/ref?)    (map :key)) conf)
             :refsets (postwalk-into #{} (comp (filter ig/refset?) (map :key)) conf)
             :id      (#'ig/normalize-key k)
             :name    (pr-str k)})
       config))

(defn un-normalizer [config]
  (into {} (juxt ::id ::key) (config->nodes config)))

(defn assoc-edge [edges ])

(defn derivee?
  [key candidate]
  (ig/derived-from? candidate key))

(defn ancestors-inclusive [k]
  (conj (ancestors k) k))

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
  (select-keys config (transitive-deps-inclusive config ks {:include-refsets?
                                                            include-refsets?})))

(defn dot [config {:keys [hierarchy selected-components derived-attrs node]
                   :or   {hierarchy @#'clojure.core/global-hierarchy
                          node      {:shape :oval}}}]
  (let [conf (cond-> config
               (seq selected-components)
               (select-components selected-components {:include-refsets? false}))]
    (tangle/graph->dot
     (config->nodes conf)
     (config->edges conf)
     {:node             node
      :directed?        true
      :node->id         ::id
      :node->descriptor (fn [{k    ::key
                              n    ::name
                              conf ::config
                              :as  x}]
                          (merge {:label (str n "\n" (pp-str x))}
                                 (merge-derivees derived-attrs k)))
      :edge->descriptor (fn [src dest opts]
                          {:label (name dest)})})))


(comment
  (require '[kafka :refer [config]])

  (methods ig/init-key)

  (-> config
      (dot {;; :selected-components #{:kafka/server}
            :derived-attrs {:kafka/topic {:color  :red
                                          :shape  :box
                                          :height 0.5
                                          :width  4}
                            :kafka/db    {:shape :cylinder}}})
      (create-img "kafka-sys3.png"))

  (def *s
    (eog "kafka-sys.png"))

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

  @*s

  )

(comment

  (println "asd")

  (defs {ps :parents
         as :ancestors}
    @#'clojure.core/global-hierarchy)

  ;; Then a mapping between participant keys and actual components.

  (-> (tangle/graph->dot
       {}
       (dependencies->edges ps)
       {:node             {:shape :oval}
        :directed?        true
        :node->id         pr-str
        :node->descriptor (fn [n] {:label (pr-str n)})})
      (create-img "examples/hello.png"))

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
