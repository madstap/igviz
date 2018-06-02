(ns madstap.igviz
  (:require
   [integrant.core :as ig]
   [dorothy.core :as dot]
   [dorothy.jvm :refer [save! show!]]
   [clojure.java.browse :refer [browse-url]]
   [clojure.java.io :as io]
   [madstap.comfy :as comfy])
  (:import
   (java.io File)))

(defn compile-graph [config]
  (reduce-kv (fn [acc k v]
               (comfy/postwalk-transduce
                (comp (filter ig/ref?) (map :key))
                (completing #(conj %1 [k %2]))
                acc, v))
             [], config))

(defn visualize
  "Vizualize an integrant configuration.

  Options:

  :format
  The output format, defaults to :pdf (other valid options are :png and :svg)

  :open?
  Whether to open the generated image. Defaults to true.

  :save-as
  When provided, save the generated image to this location."
  ([config] (visualize config {}))
  ([config {:keys [format open? save-as]
            :or {format :pdf
                 open? true}
            :as opts}]
   (let [file (File/createTempFile "igviz-" (str "." (name format)))
         url (.toURL file)
         dot (-> (compile-graph config) (dot/digraph) (dot/dot))]

     (save! dot file {:format format})

     (when save-as
       (save! dot save-as {:format format}))

     (when open? (browse-url url))

     nil)))
