(ns madstap.igviz2
  (:require
   [clojure.java.shell :as sh]
   [integrant.core :as ig]
   [integrant.repl :as ig.repl]
   [tangle.core :as tangle]
   [clojure.java.browse :refer [browse-url]]
   [clojure.java.io :as io]
   [medley.core :as medley]
   [madstap.comfy :as comfy :refer [defs]]
   [clojure.java.io :as io]))

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
  (-> config ig/dependency-graph :dependencies (dependencies->edges #'ig/normalize-key)))

(defn config->nodes [config]
  (into {}
        (map (fn [k]
               (let [normalized (#'ig/normalize-key k)]
                 [normalized
                  {::key        k
                   ::normalized normalized
                   ::name       (pr-str k)}])))
        (keys config)))

(def k :kafka/topic)

(defn derived-ids [config k]
  (->> (ig/find-derived config k)
       (map first)
       (map #'ig/normalize-key)))

(comment
  (defn update-derived-nodes [nodes k f & args]
    (reduce (fn [nds derived-k]
              (apply update nds derived-k f args))
            nodes
            (derived-ids nodes k))))

(defn partialf
  "Given a function f and one less than the normal arguments of f as args,
  returns a function of one argument x that invokes (f x arg1 arg2 argN...)"
  [f & args]
  (fn [x]
    (apply f x args)))

(defn derivee?
  [key candidate]
  (ig/derived-from? candidate key))

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

;; TODO: better name, generalize
(defn system-dot
  ([config node->descriptor]
   (system-dot config @#'clojure.core/global-hierarchy node->descriptor))
  ([config hierarchy node->descriptor]
   (tangle/graph->dot
    (config->nodes config)
    (config->edges config)
    {:node             {:shape :oval}
     :directed?        true
     :node->id         (fn [[id _]] id)
     :node->descriptor (fn [[_ {k   ::key
                                n   ::name
                                :as node}]]
                         (merge {:label n} (merge-derivees node->descriptor k)))})))

(defn eog [path]
  (future (sh/sh "eog" path)))

(comment
  (require '[kafka :refer [config]])

  (-> (system-dot config {:kafka/topic {:color  :red
                                        :shape  :box
                                        :height 0.5
                                        :width  4
                                        }
                          :kafka/db    {:shape :cylinder}})
      (create-img "examples/hello.png"))

  (def *s
    (eog "examples/hello.png"))

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
