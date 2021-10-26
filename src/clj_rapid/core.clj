(ns clj-rapid.core
  (:require
   [clj-rapid.specs :as specs]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [ring.util.response :as response]))

(def http-methods #{:get :put :post :patch :delete :options :*})

(def param-sources [:body :header :query :form :form* :query* :path :request])

(def ANY-PATH :*)

(defmulti conform-to (fn [type value]
                       (cond
                         (or (s/spec? type) (and (keyword? type) (s/get-spec type))) ::spec
                         :else type)))

(defmethod conform-to ::spec [s v] (specs/conform! s v))

(defmethod conform-to String [_ v] v)

(defmethod conform-to Long [_ v] (when v (Long/parseLong v)))

(defmethod conform-to Boolean [_ v] (when v (Boolean/parseBoolean v)))

(defmethod conform-to :int [_ v] (when v (Integer/parseInt v)))

(defmethod conform-to :decimal [_ v] (when v (BigDecimal. v)))

(defmacro unless [p v exp]
  `(if (~p ~v) ~v ~exp))

(defn- path-var? [path]
  (str/starts-with? path ":"))

(defn- route [f-name f-meta]
  (let [[verb path] (or (some #(some->> (f-meta %) (vector %)) http-methods)
                        [:get (str "/" f-name)])
        path-parts (filter (comp not empty?) (str/split path #"/"))
        path-vars (->> (filterv path-var? path-parts)
                       (mapv #(subs % 1)))
        path-parts (mapv #(if (path-var? %) ANY-PATH %) path-parts)]
    {:verb verb
     :path path-parts
     :path-vars (mapv keyword path-vars)}))

(defmacro defapi
  "Defines a Ring handler function.
  Uses Clojure metadata to specify and describe the API endpoint.
  The following keys are supported:
  :get,:post,:put,:patch,:delete	The request path string including path variables. e.g /mypath/:somevar/more
  :middleware A vector of Ring middleware

  The arguments also support the following optional metadata formats:

  - `^int arg`, basic format with only type annotation
  - `^{:source :query :type int ...}`, explicit metadata supporting following keys:
      * `:source` , the request source one of `:path`, `:query`, `:form`, `:header`, `:body`. Defaults to `:query`. Additionally, the special keys `:query*` and `:form*` are used to match all the query and form parameters.
      * `:type` , the type annotation for simple values. Built-in type conversions exists for int, bool, LocalDate, LocalTime, LocalDateTime. Additional conversions can be defined implementing the protocol `FromString`
      * `:name`, the name in the request `source`, applicable for `:path`, `:query`, `:form`, `:header`. It defaults to the arg name.
      * `:valid`, a tuple of validation function or an spec and an optional message.
      * `:default`, a default value
      * `:doc`, documentation of the argument
  "
  {:arglists '([route? name docstring? [params*] body])}
  [fname & fdecl]
  (when-not (symbol? fname)
    (throw (IllegalArgumentException. "First argument to defapi must be a symbol")))
  (let [m (or (meta fname) {})
        [m fdecl] (if (string? (first fdecl))
                    [(assoc m :doc (first fdecl)) (rest fdecl)]
                    [m fdecl])
        _ (when-not (vector? (first fdecl))
            (throw (IllegalArgumentException. "Expected argument vector")))
        [argv & body] fdecl
        argv (mapv #(vary-meta % eval) argv)
        route# (route (name fname) m)]
    `(defn ~(with-meta fname
              (assoc m :api-fn? true
                     :route route#))
       ~argv
       ~@body)))

(defn- wrap-error
  [f error-type arg-meta]
  (fn [x]
    (try (f x)
         (catch Exception e
           (throw (ex-info (format "Failed conforming argument:%s to %s due to %s"
                                   (:name arg-meta) (:type arg-meta) (.getMessage e))
                           {:type error-type :arg (:name arg-meta) :cause e}))))))

(defn- arg-parser [arg path-vars]
  (let [arg-meta (as-> (meta arg) arg-meta
                   (unless :name arg-meta (assoc arg-meta :name (name arg)))
                   (unless :source arg-meta
                           (if-let [[source type] (first (select-keys arg-meta param-sources))]
                             (if (true? type)
                               (assoc arg-meta :source source)
                               (assoc arg-meta :source source :type type))
                             (assoc arg-meta
                                    :source (if (some #(= (:name arg-meta) (name %)) path-vars)
                                              :path :query))))
                   (unless :type arg-meta (assoc arg-meta :type (:tag arg-meta))))
        convert-fn (or (some->> (:type arg-meta) (partial conform-to)) identity)
        value-fn (case (:source arg-meta)
                   :body #(or (:body-params %) (:body %))
                   :query (comp (keyword (:name arg-meta)) :query-params)
                   :form (comp (keyword (:name arg-meta)) :form-params)
                   :path (comp (keyword (:name arg-meta)) :path-params)
                   :header #(get-in % [:headers (:name arg-meta)])
                   :query* :query-params
                   :form* :form-params
                   :request (keyword (:name arg-meta)))
        valid-fn (if-let [[valid msg] (:valid arg-meta)]
                   (fn [v]
                     (when-not (valid v)
                       (throw (ex-info (or msg "invalid value:" v)
                                       {:type :bad-input :value v :arg (:name arg-meta)})))
                     v)
                   identity)
        default-fn (if-let [default (:default arg-meta)]
                     (fn [v] (if (nil? v) default v))
                     identity)]
    (comp default-fn valid-fn (wrap-error convert-fn :bad-input arg-meta)  value-fn)))

(defn- fn->handler [f]
  (let [m (meta f)
        arg-parsers (mapv #(arg-parser % (get-in m [:route :path-vars])) (first (:arglists m)))
        argv-fn (if (seq arg-parsers)
                  (apply juxt arg-parsers)
                  (constantly []))
        handler (fn [req]
                  (println "apply f:" (argv-fn req))
                  (apply f (argv-fn req)))]
    [(:route m) (if-let [wrappers (:wrappers m)]
                  ((apply comp wrappers) handler)
                  handler)]))

(defn- routes [& fs]
  (let [route-handlers (map fn->handler fs)]
    (reduce (fn [result [route handler]]
              (assoc-in result
                        (cons (:verb route) (conj (:path route) :/))
                        [route handler]))
            {} route-handlers)))

(defn- match-path [routes ks path-params]
  (when routes
    (some-> (or
             (get-in routes ks)
             (match-path (get routes (first ks)) (rest ks) path-params)
             (match-path (get routes :*) (rest ks) (conj path-params (first ks))))
            (conj path-params))))

(defn- match-request [routes req]
  (let [verb (:request-method req)
        path (cons verb (-> req :uri (str/split #"/")
                            ((partial filterv not-empty))
                            (conj :/)))]
    (match-path routes path [])))

(defn- routes-in [ns]
  (apply routes (->> (ns-interns ns)
                     (map second)
                     (filter (comp :api-fn? meta)))))

(defn- swagger-spec [ns format info]
  {:openapi "3.0.3"
   :info info
   :servers {:url ""}}
  )

(defn- wrap-response [f]
  (fn [req]
    (try
      (response/response (f req))
      (catch Exception e
        (if (= :bad-input (:type (ex-data e)))
          (response/bad-request (assoc (ex-data e) :message (ex-message e)))
          (response/status (ex-data e) 500))))))

(defn handler
  "Generates a composite handler using the `defapi` definitions in the specified namespace.
  This handler can be used for serving requests"
  [ns]
  (let [routes (routes-in ns)]
    (fn [req]
      (let [[route route-fn path-params] (match-request routes req)
            req (assoc req :path-params (zipmap (:path-vars route) path-params))]
        (when route-fn
          ((wrap-response route-fn) req))))))
