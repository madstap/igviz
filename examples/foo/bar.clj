(ns foo.bar
  (:require
   [integrant.core :as ig]
   [madstap.igviz :as igviz]))

(def my-system
  {::config :foo
   ::db {:config (ig/ref ::config)}
   ::server {:db (ig/ref ::db)
             :config (ig/ref ::config)}})

(comment
  (igviz/visualize my-system)
  )
