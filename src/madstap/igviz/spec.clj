(ns madstap.igviz.spec
  (:require [clojure.alpha.spec :as s]
            [integrant.core :as ig]))

(s/def :igviz/config
  (s/map-of :igviz/key any?))

(s/def :igviz/system
  (s/and map? #(s/valid? :igviz/config (#'ig/system-origin %))))

(s/def :igviz/ref
  ig/reflike?)

(s/def :igviz/refs
  (s/coll-of :igviz/ref :kind set?))

(s/def :igviz/node
  (s/keys :req [:igviz.node/id
                :igviz.node/key
                :igviz/refs]))

(s/def :igivz.edge/edge
  (s/tuple :igviz.edge/src :igviz.edge/dest))

(s/def :igviz/edge
  (s/keys :req [:igviz.edge/id
                :igviz.edge/src
                :igviz.edge/dest
                :igviz.edge/edge
                :igviz/refs]))

(s/def :igviz/nodes
  (s/coll-of :igviz/node :kind set?))

(s/def :igviz/edges
  (s/coll-of :igviz/edge :kind set?))

(s/def :igviz/graph
  (s/keys :req [:igviz/nodes :igviz/edges]))

(s/def :igviz.selected/nodes
  (s/coll-of :igviz.node/key :kind set?))

(s/def :igviz.selected/edges
  (s/coll-of :igviz.edge/edge :kind set?))

(s/def :igviz/simple-key
  keyword?)

(s/def :igviz/composite-key
  (s/coll-of :igviz/simple-key :kind vector? :min-count 1))

(s/def :igviz/key
  (s/or :simple :igviz/simple-key
        :composite :igviz/composite-key))

(s/def :igviz/selector
  keyword?)

(s/def :igviz/transform
  keyword?)

(s/def :igviz/transformation
  (s/tuple :igviz/selector :igviz/trans))



;; (s/def :igviz/rule
;;   (s/tuple any? any?))

;; (s/def :igviz/rules
;;   (s/coll-of
;;    (s/tuple :igviz/selector
;;             (s/coll-of (s/tuple :igviz/transform any?)))
;;    :kind (some-fn map? vector?)))
