(ns clj-rapid.specs
  (:require [clojure.spec.alpha :as s]))

(defn some-spec? [x]
  (or (s/spec? x) (s/get-spec x)))

(s/def ::arg-meta-key #{:param :query :path :form :body :header})

(s/def ::name string?)
(s/def ::default any?)
;; (s/def ::valid (s/tuple fn? string?))
(s/def ::valid (s/tuple any? string?))
(s/def ::valid-fn any?)
(s/def ::spec some-spec?)
(s/def ::doc string?)

(s/def ::arg-props (s/keys :opt-un [::name ::default ::valid-fn ::valid ::spec ::doc]))

(s/def ::arg-meta-value (s/or :name ::name
                              :spec ::spec
                              :valid-fn ::valid-fn
                              :valid ::valid
                              :props ::arg-props))

(s/def ::arg-meta (s/tuple ::arg-meta-key ::arg-meta-value ))

(defn conform! [a-spec a-value]
  (let [result (s/conform a-spec a-value)]
    (when (s/invalid? result)
      (throw (ex-info (format "value failed to conform to spec: %s"
                              (s/explain-str a-spec a-value)) {})))
    result))

(s/def ::handler-fn-args (s/alt :ns-symbol #(instance? clojure.lang.Namespace %)
                           :fn-vars (s/* var?)
                           :nested-handelrs (s/* (s/cat :prefix string? :handler fn?))))
