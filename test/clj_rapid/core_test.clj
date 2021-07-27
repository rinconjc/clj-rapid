(ns clj-rapid.core-test
  (:require [clj-rapid.core :refer :all]
            [clj-rapid.util :as util]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.string :as str])
  (:import java.util.Date))

(s/def ::title string?)

(s/def ::item (s/keys :req-un [::title] ))

(defapi ping []
  (str "Hello at " (Date.)))

(defapi ^{:get "/hello/:name"} hello
  "This will say hello "
  [name]
  (str "Hello:" name))

(defapi ^{:post "/item"} post-item
  "Creates a TODO item"
  [^{:body ::item} item]
  (assoc item :created (Date.) :id 1))

(defapi ^{:get "/items"} search-item
  "Search todo items"
  [^{:query string?} text
   ^{:query {:default 10 :valid [int? "Should be an integer"]}} limit
   ^{:query {:default 0 :valid [int? "Should be an integer"]}} offset]
  {:count 1
   :items [{:todo "Buy milk"}]})

(defapi ^{:get "/find-items"} find-items
  "Find items using a query"
  [^{:query ::item-query} query])

(def routes  #'clj-rapid.core/routes)

(def match-request #'clj-rapid.core/match-request)

(deftest routing
  (testing "route matching"
    (let [ping-router (routes #'clj-rapid.core-test/ping)]
      (is (some? ping-router))
      (println "route:" ping-router)
      (doseq [method [:get :post :put :patch :delete]]
        (is (util/alike? [map? fn? vector?]
                         (match-request ping-router {:uri "/ping" :request-method method})))))))

(def this-ns *ns*)

(deftest request-handling
  (testing "handling requests"
    (let [req-handler (handler this-ns)]
      (is (str/starts-with? (req-handler {:request-method :get :uri "/ping"}) "Hello at")))))
