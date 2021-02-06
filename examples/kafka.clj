(ns kafka
  (:require [integrant.core :as ig]
            [madstap.igviz.alpha :as igviz]
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

(derive ::consumer2 ::foo)

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
                             :consumer (ig/ref ::consumer1)
                             :foo (ig/ref ::bar-topic)}
   [::foo-topic ::topic]    {:topic-name "foo"}
   [::bar-topic ::topic]    {:topic-name "bar"}
   [::baz-topic ::topic]    {:topic-name "baz"}})

(comment

  (igviz/viz sconf-new (igviz/diff sconf-old) {:open? true})

  (def rules
    (igviz/comp
     ;;     (diff sconf-old)
     igviz/label-edges
     {:derived {::topic {:merge-attrs     {:shape  :box
                                           :height 0.5
                                           :width  4}
                         :show-config [:topic-name]}
                ::db    {:merge-attrs {:shape :cylinder}
                         :show-config [:url :wat]}}
      :ks      {::consumer1 {:merge-attrs {:color :red}}
                ;; ::error-component :select
                }}
     ))


  (igviz/viz config rules {:open? true})


  (igviz/viz
   config
   {:derived {;; ::consumer1 :remove
              ;; ::error-component :remove
              ;; ::server :remove
              [:and ::consumer ::foo] {:merge-attrs {:color :blue}}
              }
    :related {[:or ::consumer1 ::producer] {:merge-attrs {:color :red}}}
    }
   {:open? true})

  (ig/init config)

  (future
    (igviz/visualize config))

  @*1


  )
