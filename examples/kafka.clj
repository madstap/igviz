(ns kafka
  (:require [integrant.core :as ig]
            [madstap.igviz :as igviz]
            [medley.core :as medley]))

(defmacro noop-keys [& ks]
  `(do ~@(map (fn [k] `(defmethod ig/init-key ~k ~@'[[_ x] x])) ks)))

(noop-keys ::db ::server ::cache ::consumer ::producer ::topic
           ::error-component ::success-component ::elasticsearch)

(derive ::error-component :duct/daemon)
(derive ::success-component :duct/daemon)

(def sconf-old
  {::db     {:url     "..."
             :db-name "foo-db"}
   ::cache  {:url "..."}
   ::server {:port  1234
             :db    (ig/ref ::db)
             :cache (ig/ref ::cache)}})

(def sconf-new
  (-> sconf-old
      (dissoc ::cache)
      (medley/dissoc-in [::server :cache])
      (assoc ::elasticsearch {:url "..."})
      (assoc-in [::server :elastic] (ig/ref ::elasticsearch))))

(def config
  {::db                     {:url "..."}
   ::cache                  {:url "..."}
   ::server                 {:db         (ig/ref ::db)
                             :cache      (ig/ref ::cache)
                             :daemons    (ig/refset :duct/daemon)
                             :all-topics (ig/refset ::topic)
                             ;; :all-consumers (ig/refset ::consumer)
                             }
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
