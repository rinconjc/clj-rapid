(ns clj-rapid.core
  (:require
   [clj-rapid.specs :as specs]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [muuntaja.middleware :as mm]
   [ring.middleware.params :as params]
   [ring.util.response :as response]))

(def http-methods #{:get :put :post :patch :delete :options :*})

(def param-sources [:body :header :query :form :form* :query* :path :request :param :param*])

(def ANY-PATH :*)

(defmulti conform-to (fn [type _]
                       (cond
                         (or (s/spec? type) (and (keyword? type) (s/get-spec type))) ::spec
                         :else type)))

(defmethod conform-to ::spec [s v] (specs/conform! s v))

(defmethod conform-to String [_ v] v)

(defmethod conform-to Long [_ v] (when v (Long/parseLong v)))

(defmethod conform-to Boolean [_ v] (when v (Boolean/parseBoolean v)))

(defmethod conform-to :int [_ v] (when v (Integer/parseInt v)))

(defmethod conform-to :decimal [_ v] (when v (BigDecimal. v)))

(defmethod conform-to :default
  [type _]
  (log/warnf "No type conformer defined for %s" type))

(defmacro unless [p v exp]
  `(if (~p ~v) ~v ~exp))

(defn- path-var? [path]
  (str/starts-with? path ":"))

(defn- route [f-var]
  (let [f-meta (meta f-var)]
    (when-let [[method path] (some #(some->> (f-meta %) (vector %)) http-methods)]
      (let [path (if (true? path) (str (:name f-meta)) path)
            path-parts (filter (comp not empty?) (str/split path #"/"))
            path-vars (->> (filterv path-var? path-parts)
                           (mapv #(subs % 1)))
            path-parts (mapv #(if (path-var? %) ANY-PATH %) path-parts)]
        {:method method
         :path path-parts
         :path-vars (mapv keyword path-vars)}))))

(defn- wrap-error
  [f error-type arg-meta]
  (fn [x]
    (try
      (f x)
      (catch Exception e
        (log/warn "type coercion error for value" x "with function" f)
        (throw (ex-info (format "Failed conforming argument: %s to %s due to %s"
                                (:name arg-meta) (:type arg-meta) (.getMessage e))
                        {:type error-type :arg (:name arg-meta)}
                        e))))))

(defn- arg-parser [arg path-vars]
  (let [arg-meta (as-> (meta arg) arg-meta
                   (unless :name arg-meta (assoc arg-meta :name (name arg)))
                   (update arg-meta :name keyword)
                   (unless :source arg-meta
                           (if-let [[source type] (first (select-keys arg-meta param-sources))]
                             (if (true? type)
                               (assoc arg-meta :source source)
                               (assoc arg-meta :source source :type type))
                             (assoc arg-meta
                                    :source (if (some #(= (:name arg-meta) %) path-vars)
                                              :path :query))))
                   (unless :type arg-meta (assoc arg-meta :type (:tag arg-meta))))
        convert-fn (or (some->> (:type arg-meta) (partial conform-to)) identity)
        value-fn (case (:source arg-meta)
                   :body #(or (:body-params %) (:body %))
                   :query #(get-in % [:query-params (:name arg-meta)])
                   :form #(get-in % [:form-params (:name arg-meta)])
                   :param #(get-in % [:params (:name arg-meta)])
                   :params :params
                   :path (comp (:name arg-meta) :path-params)
                   :header #(get-in % [:headers (:name arg-meta)])
                   :query* :query-params
                   :form* :form-params
                   :request (:name arg-meta))
        default-fn (if-let [default (:default arg-meta)]
                     (fn [v] (if (nil? v) default v))
                     identity)]
    (comp default-fn (wrap-error convert-fn :bad-input arg-meta) value-fn)))

(defn- route-handler [f-var]
  (when-let [r (route f-var)]
    (let [m (meta f-var)
          arg-parsers (mapv #(arg-parser % (:path-vars r)) (first (:arglists m)))
          argv-fn (if (seq arg-parsers)
                    (apply juxt arg-parsers)
                    (constantly []))
          handler (fn [req]
                    (apply f-var (argv-fn req)))]
      [r (if-let [wrappers (:wrappers m)]
           ((apply comp wrappers) handler)
           handler)])))

(defn- merge-routes [routes [route handler]]
  (assoc-in routes
            (cons (:method route) (conj (:path route) :/))
            [route handler]))

(defn- routes-from [fn-vars]
  (reduce merge-routes {}
          (->> fn-vars
               (map route-handler)
               (filter some?))))

(defn- ns-routes [ns]
  (routes-from (->> (ns-interns ns) (map second))))

(defn- match-path [routes ks path-params]
  (when routes
    (some-> (or
             (get-in routes ks)
             (match-path (get routes (first ks)) (rest ks) path-params)
             (match-path (get routes :*) (rest ks) (conj path-params (first ks))))
            (conj path-params))))

(defn- match-request [routes req]
  (let [method (:request-method req)
        path (cons method (-> req :uri (str/split #"/")
                            ((partial filterv not-empty))
                            (conj :/)))]
    (match-path routes path [])))

(defn- swagger-spec [ns format info]
  {:openapi "3.0.3"
   :info info
   :servers {:url ""}})

(defn- wrap-response [f]
  (fn [req]
    (try
      (let [result (f req)]
        (if (response/response? result)
          result
          (response/response result)))
      (catch Exception e
        (println e)
        (if (= :bad-input (:type (ex-data e)))
          (response/bad-request (assoc (ex-data e) :message (ex-message e)))
          (throw e))))))

(defn- handler-from [routes]
  (let [req-handler (fn [req]
                      (let [[route route-fn path-params] (match-request routes req)
                            req (assoc req :path-params (zipmap (:path-vars route) path-params))]
                        (if route-fn
                          (route-fn req)
                          (response/not-found {}))))]
    (-> req-handler
        params/wrap-params
        wrap-response
        mm/wrap-format)))

(defn- normalise-prefix [uri-prefix]
  (as-> uri-prefix prefix
    (if (str/ends-with? prefix "/") prefix (str prefix "/"))
    (if (str/starts-with? prefix "/") prefix (str "/" prefix))))

(defn- composite-handler [prefix-and-handlers]
  (let [prefix-and-handlers (map #(vector (normalise-prefix (first %)) (second %)) prefix-and-handlers)]
    (fn [{:keys [uri] :as req}]
      (if-let [[prefix handler-fn] (some #(when (str/starts-with? uri (first %)) %) prefix-and-handlers)]
        (handler-fn (update req :uri subs (dec (count prefix))))
        (response/not-found {})))))

(defn handler
  "Creates a handler using the definitions in the specified namespace.
  This handler can be used for serving requests"
  {:arglists '([ns-symbol]
               [fn-var & more-fn-vars]
               [[prefix handler] & more-prefix-and-handlers])}
  [ns-or-fn-or-pair & more-args]
  (cond
    (vector? ns-or-fn-or-pair) (composite-handler (cons ns-or-fn-or-pair more-args))
    (instance? clojure.lang.Namespace ns-or-fn-or-pair) (handler-from (ns-routes ns-or-fn-or-pair))
    :else (handler-from (routes-from (cons ns-or-fn-or-pair more-args)))))

(s/fdef handler
  :args ::specs/handler-fn-args
  :ret fn?)
