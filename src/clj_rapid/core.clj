(ns clj-rapid.core
  (:require [clj-rapid.specs :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def http-methods #{:get :put :post :patch :delete :options :*})

(def ANY-PATH :* )

(defmulti from-str (fn [type value] type))

(defmethod from-str Long [_ v] (Long/parseLong v))

(defmethod from-str Boolean [_ v] (Boolean/parseBoolean v))

(defmethod from-str int [_ v] (Integer/parseInt v))

(defmethod from-str :decimal [_ v] (BigDecimal. v))

(defmacro unless [p v exp]
  `(if (~p ~v) ~v ~exp))

;; (defn- arg-with-meta [a path-vars]
;;   (let [arg-meta (first (into [] (meta arg)))
;;         [meta-key meta-val] (when arg-meta (specs/conform! ::specs/arg-meta arg-meta))
;;         meta-val (if (= :props (first meta-val))
;;                    (second meta-val)
;;                    (into {} [meta-val]))
;;         meta-val (merge {:name (name arg)} meta-val)
;;         meta-key (or meta-key (cond
;;                                 (some #(= (:name meta-val) %) path-vars) :path
;;                                 :else :query))]
;;     (vary-meta a )))



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
     :vars path-vars}))

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
        route# (route (name fname) m)]
    `(defn ~(with-meta fname
              (assoc m :api-fn? true
                     :route route#))
       ~argv
       ~@body)))

(defn- arg-parser [arg path-vars]
  (let [arg-meta (as-> (meta arg) arg-meta
                   (unless :type arg-meta (assoc arg-meta :type (:tag arg-meta)))
                   (unless :name arg-meta (assoc arg-meta :name (name arg)))
                   (unless :source arg-meta
                           (assoc arg-meta
                                  :source (if (some #(= (:name arg-meta) %) path-vars) :path :query))))
        convert-fn (or (some->> (:type arg-meta) (partial from-str)) identity)
        value-fn (case (:source arg-meta)
                   :body :body
                   :query (comp convert-fn (keyword (:name arg-meta)) :query-params)
                   :form (comp convert-fn (keyword (:name arg-meta)) :form-params)
                   :path (comp convert-fn (keyword (:name arg-meta)) :path-params)
                   :header #(convert-fn (get-in % [:headers (:name arg-meta)]))
                   :query* :query-params
                   :form* :form-params)
        [valid msg] (or (:valid arg-meta) [identity])]
    (cond
      (fn? valid) (comp valid value-fn)
      (or (s/spec? valid) (s/get-spec valid)) (comp (partial specs/conform! valid) value-fn))))

(defn- fn->handler [f]
  (let [m (meta f)
        arg-parsers (mapv #(arg-parser % (get-in m [:route :path-vars])) (first (:arglists m)))
        argv-fn (if (seq arg-parsers)
                  (apply juxt arg-parsers)
                  (constantly []))]
    [(:route m) (fn [req]
                  (println "apply f:" (argv-fn req))
                  (apply f (argv-fn req)))]))

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
    (match-path routes path [])
    ;; (or (get-in routes path)
    ;;     (loop [routes routes
    ;;            [next & path] path
    ;;            path-params []]
    ;;       (when routes
    ;;         (let [next-routes (get routes next)]
    ;;           (if (= :/ next)
    ;;             (conj next-routes path-params)
    ;;             (recur (or next-routes (get routes :*))
    ;;                    path
    ;;                    (if (or next-routes (keyword? next)) path-params next)))))))
    ))

(defn- routes-in [ns]
  (apply routes (->> (ns-interns ns)
                     (map second)
                     (filter (comp :api-fn? meta)))))

(defn handler
  "Generates a composite handler using the `defapi` definitions in the specified namespace.
  This handler can be used for serving requests"
  [ns]
  (let [routes (routes-in ns)]
    (fn [req]
      (let [[route route-fn path-params] (match-request routes req)
            req (assoc req :path-params (zipmap (:path-vars route) path-params))]
        (when route-fn
          (route-fn req))))))
