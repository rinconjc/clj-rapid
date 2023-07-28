(ns clj-rapid.util
  (:require  [clojure.test :as t]))

(defn alike? [x y]
  (let [result* (cond
                  (map? x) (and (map? y)
                                (let [alike* (some (fn [[k v]]
                                                     (let [r* (alike? v (get y k))]
                                                       (when-not (true? r*)
                                                         r*))) x)]
                                  (or (nil? alike*) alike*)))
                  (coll? x) (and (coll? y) (or (and (empty? x) (empty? y))
                                               (let [alike* (map alike? x y)]
                                                 (or (every? true? alike*)
                                                     (filter (comp not true?) alike*)))))
                  (fn? x) (if (fn? y)
                            (identical? x y)
                            (x y))
                  :else (= x y))]
    (if (false? result*)
      [x y]
      result*)))

(defmethod t/assert-expr 'like? [msg form]
  (let [expected (nth form 1)
        expr (nth form 2)]
    `(let [expected# ~expected
           actual# ~expr
           res# (alike? expected# actual#)]
       (if (true? res#)
         (t/do-report {:type :pass :message ~msg :expected expected# :actual actual#})
         (t/do-report {:type :fail :message ~msg :expected (first res#) :actual (second res#)})))))
