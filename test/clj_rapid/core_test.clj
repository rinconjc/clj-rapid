(ns clj-rapid.core-test
  (:require [clj-rapid.core :refer :all]
            [clj-rapid.util :as util]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import java.util.Date))

(s/def ::title string?)

(s/def ::item (s/keys :req-un [::title]))

(defn wrap-user [handler]
  (fn [req]
    (if-let [user-id (-> req :session :user-id)]
      (let [user {:id user-id}]
        (handler (assoc req :user user)))
      (handler req))))

(defn ^:get ping []
  (str "Hello at " (Date.)))

(defn ^{:get "/hello/:name"} hello
  "This will say hello "
  [name]
  (str "Hello:" name))

(defn ^{:post "/item"} post-item
  "Creates a TODO item"
  [^{:body ::item} item]
  (println "post-item called...with ", item)
  (assoc item :created (Date.) :id 1))

(defn ^{:get "/items" :wrappers [wrap-user]} search-item
  "Search TODO items"
  [^:request user
   ^:query text
   ^{:query :int :default 10} limit
   ^{:query :int :default 0} offset]
  {:count limit
   :offset offset
   :items [{:todo "Buy milk"}]
   :user user})

(defn ^{:get "/find-items"} find-items
  "Find items using a query"
  [^{:query ::item-query} query])

(def routes-from  #'clj-rapid.core/routes-from)

(def match-request #'clj-rapid.core/match-request)

(deftest routing
  (testing "route matching"
    (let [ping-router (routes-from [#'clj-rapid.core-test/ping])]
      (is (some? ping-router))
      (is (some? (get-in ping-router [:get "ping"])))
      (println "route:" ping-router)
      (doseq [method [:get :post :put :patch :delete]]
        (is (util/alike? [map? fn? vector?]
                         (match-request ping-router {:uri "/ping" :request-method method})))))))

(defn read-json [input]
  (json/read-str (slurp input) :key-fn keyword))

(def this-ns *ns*)

(deftest request-handling
  (testing "handling requests"
    (let [req-handler (handler "/" this-ns)]
      (is (like? {:status 200 :body #(str/starts-with? % "Hello at")}
                 (req-handler {:request-method :get :uri "/ping"})))
      (is (= "Hello:there" (:body (req-handler {:request-method :get :uri "/hello/there"}))))
      (is (like? {:body (fn [body] (util/alike? {:id 1 :title "the title"} (read-json body))) :status 200}
                 (req-handler {:request-method :post :uri "/item" :body {:title "the title"}})))
      (is (like? {:status 400 :body (fn [body]
                                      (util/alike? {:message #(str/starts-with? % "Failed conforming")}
                                                   (read-json body)))}
                 (req-handler {:request-method :post :uri "/item" :body {:name "the title"}})))
      (is (like? {:status 200
                  :body (fn [body]
                          (util/alike? {:count 20
                                        :offset 10
                                        :items [{:todo "Buy milk"}]} (read-json body)))}
                 (req-handler {:request-method :get
                               :uri "/items"
                               :query-params {:text "shopping" :limit "20" :offset "10"}}))))))
