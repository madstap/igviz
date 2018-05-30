# igviz

Integrant visualization.

## Usage

Dependency `[igviz "0.1.0"]`

```clojure
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
```


## License

Copyright © 2018 Aleksander Madland Stapnes

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
