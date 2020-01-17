(ns kafka
  (:require [integrant.core :as ig]
            [madstap.igviz :as igviz]))

(defmacro noop-keys [& ks]
  `(do ~@(map (fn [k] `(defmethod ig/init-key ~k ~@'[[_ x] x])) ks)))

(noop-keys ::db ::server ::cache ::consumer ::producer ::topic
           ::error-component ::success-component)

(derive ::error-component :duct/daemon)
(derive ::success-component :duct/daemon)

(def config
  {::db                     {:url "..."}
   ::cache                  {:url "..."}
   ::server                 {:db         (ig/ref ::db)
                             :cache      (ig/ref ::cache)
                             :daemons    (ig/ref :duct/daemon)
                             :all-topics (ig/refset ::topic)}
   [::consumer1 ::consumer] {:topics [(ig/ref ::foo-topic)
                                      (ig/ref ::bar-topic)]}
   [::consumer2 ::consumer] {:topics [(ig/ref ::foo-topic)
                                      (ig/ref ::bar-topic)
                                      (ig/ref ::baz-topic)]}
   ::producer               {:topic (ig/ref ::baz-topic)}
   ::error-component        {:producer (ig/ref ::producer)
                             :consumer (ig/ref ::consumer1)}
   [::foo-topic ::topic]    {:topic-name "foo"}
   [::bar-topic ::topic]    {:topic-name "bar"}
   [::baz-topic ::topic]    {:topic-name "baz"}})

(comment

  (ig/init config)

  (future
    (igviz/visualize config))

  @*1


  )
