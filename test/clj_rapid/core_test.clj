(ns clj-rapid.core-test
  (:require [clj-rapid.core :refer :all]
            [clj-rapid.util :as util]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.string :as str])
  (:import java.util.Date))

(s/def ::title string?)

(s/def ::item (s/keys :req-un [::title]))

(defn wrap-user [handler]
  (fn [req]
    (if-let [user-id (-> req :session :user-id)]
      (let [user {:id user-id}]
        (handler (assoc req :user user)))
      (handler req))))

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

(defapi ^{:get "/items" :wrappers [wrap-user]} search-item
  "Search TODO items"
  [^:request user
   ^{:query String} text
   ^{:query :int :default 10 :valid [int? "Should be an integer"]} limit
   ^{:query :int :default 0 :valid [int? "Should be an integer"]} offset]
  {:count limit
   :offset offset
   :items [{:todo "Buy milk"}]
   :user user})

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
      (is (like? {:status 200 :body #(str/starts-with? % "Hello at")}
                 (req-handler {:request-method :get :uri "/ping"})))
      (is (= "Hello:there" (:body (req-handler {:request-method :get :uri "/hello/there"}))))
      (is (like? {:body {:id 1 :title "the title"}}
                 (req-handler {:request-method :post :uri "/item" :body {:title "the title"}})))
      (is (like? {:status 400 :body {:message #(str/starts-with? % "Failed conforming")}}
                 (req-handler {:request-method :post :uri "/item" :body {:name "the title"}})))
      (is (like? {:status 200
                  :body {:count 20
                         :offset 10
                         :items [{:todo "Buy milk"}]}}
                 (req-handler {:request-method :get
                               :uri "/items"
                               :query-params {:text "shopping" :limit "20" :offset "10"}}))))))
