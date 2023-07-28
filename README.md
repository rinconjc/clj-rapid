# clj-rapid
[![Build Status](https://travis-ci.org/clj-rapid/clj-rapid.svg?branch=master)](https://travis-ci.org/clj-rapid/clj-rapid)
[![codecov](https://codecov.io/gh/clj-rapid/clj-rapid/branch/master/graph/badge.svg)](https://codecov.io/gh/clj-rapid/clj-rapid)
[![Clojars Project](https://img.shields.io/clojars/v/clj-rapid.svg)](https://clojars.org/clj-rapid)


An simple routing library for Clojure, inspired by Python's Fast API and Rust Axum's route definitions.

```clj
[clj-rapid "0.0.0"]
```


## Usage

### Route Definitions

A function using a implicit path derived from the fn name:

```clojure
;; Defines GET /ping
(defn ^:get ping []
    "up & runnig")
```

With explicit path:
```clojure
(defn ^{:get "/posts"} get-posts []
    [{:id 1 :title "post 1"}])
```

With path parameters:

```clojure
(defn ^{:get "/posts/:id"} get-post [id]
    ;; (get-post-by-id id)
    )
```

With parameter de-structuring:

```clojure
(defn ^{:get "/posts"} get-posts [^:param offset ^:param limit]
    ;; (get-posts offset limit)
    )
```

With body payload:

```clojure
(defn ^{:post "/posts"} new-post [^:body post]
    ;;(create-post post)
    )
```

With spec validation:

```clojure
;; ...
(s/def ::post (s/keys :req-un [::title ::content]))

(defn ^{:post "/posts"} new-post [^{:body ::post} post]
    ;;(create-post post)
    )
```

```clojure
;; ...
(s/def ::search-params (s/keys :req-un [::limit ::offset]))

(defn ^{:get "/posts"} get-posts [^{:param* ::search-params} params]
    ;;(create-post post)
    )
```

With known type coercion/validation:

```clojure
(defn ^{:get "/posts"} get-posts
    "retrieves up to `limit` number of posts starting in the given `offset`"
    [^{:param int} offset
    ^{:param int} limit]
    ;; (get-posts offset limit)
    )
```

With defaults

```clojure
(defn ^{:get "/posts"} get-posts
    "retrieves up to `limit` number of posts starting in the given `offset`"
    [^{:param int :default 0} offset
    ^{:param int :default 10} limit]
    ;; (get-posts offset limit)
    )
```

### Converting to Ring Handler

Creating a handler from all router functions in a namespace:

```clojure
(def current-ns *ns*)
(handler "/" current-ns)
```

Creating a handler from a list of router functions

```clojure
(handler "/api" [get-posts new-post])
```


## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
