;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.schema
  (:refer-clojure :exclude [deref merge])
  #?(:cljs (:require-macros [app.common.schema :refer [ignoring]]))
  (:require
   [app.common.data :as d]
   [app.common.schema.generators :as sg]
   [app.common.schema.openapi :as-alias oapi]
   [app.common.schema.registry :as sr]
   [app.common.uuid :as uuid]
   [clojure.test.check.generators :as tgen]
   [cuerdas.core :as str]
   [malli.core :as m]
   [malli.error :as me]
   [malli.dev.pretty :as mdp]
   [malli.generator :as mg]
   [malli.registry :as mr]
   [malli.transform :as mt]
   [malli.util :as mu]))

(defn validate
  [s value]
  (m/validate s value {:registry sr/default-registry}))

(defn explain
  [s value]
  (m/explain s value {:registry sr/default-registry}))

(defn explain-data
  [s value]
  (mu/explain-data s value {:registry sr/default-registry}))

(defn schema?
  [o]
  (m/schema? o))

(defn schema
  [s]
  (m/schema s {:registry sr/default-registry}))

(defn humanize
  [exp]
  (me/humanize exp))

(defn generate
  ([s]
   (mg/generate (schema s)))
  ([s o]
   (mg/generate (schema s) o)))

(defn generate
  ([s]
   (mg/generate (schema s)))
  ([s o]
   (mg/generate (schema s) o)))

(defn form
  [s]
  (m/form s))

(defn merge
  [& items]
  (apply mu/merge (map schema items)))

(defn ref?
  [s]
  (m/-ref-schema? s))

(defn deref
  [s]
  (m/deref s))

(defn error-values
  [exp]
  (malli.error/error-value exp {:malli.error/mask-valid-values '...}))

(def default-transformer
  (let [default-decoder
        {:compile (fn [s _registry]
                    (let [props (m/type-properties s)]
                      (::oapi/decode props)))}
        default-encoder
        {:compile (fn [s _]
                    (let [props (m/type-properties s)]
                      (::oapi/encode props)))}

        coders {:vector mt/-sequential-or-set->vector
                :sequential mt/-sequential-or-set->seq
                :set mt/-sequential->set
                :tuple mt/-sequential->vector}]

    (mt/transformer
     {:name :penpot
      :default-decoder default-decoder
      :default-encoder default-encoder}
     {:name :string
      :decoders (mt/-string-decoders)
      :encoders (mt/-string-encoders)}
     {:name :collections
      :decoders coders
      :encoders coders}

     )))

(defn validator
  [s]
  (-> s schema m/validator))

(defn explainer
  [s]
  (-> s schema m/explainer))

(defmacro lazy-validator
  [s]
  `(let [vfn# (delay (validator ~s))]
     (fn [v#] (@vfn# v#))))

(defmacro lazy-explainer
  [s]
  `(let [vfn# (delay (explainer ~s))]
     (fn [v#] (@vfn# v#))))

(defn encode
  ([s val transformer]
   (m/encode s val {:registry sr/default-registry} transformer))
  ([s val options transformer]
   (m/encode s val options transformer)))

(defn decode
  ([s val transformer]
   (m/decode s val {:registry sr/default-registry} transformer))
  ([s val options transformer]
   (m/decode s val options transformer)))

(defn decoder
  ([s transformer]
   (m/decoder s  {:registry sr/default-registry} transformer))
  ([s options transformer]
   (m/decoder s options transformer)))

(defn humanize-data
  [explain-data]
  (let [errors  (humanize explain-data)
        schema  (-> explain-data :schema m/form)
        value   (-> explain-data error-values d/without-qualified)]
    (array-map
     :schema schema
     :value value
     :errors errors)))

(defn pretty-explain
  [s d]
  (mdp/explain (schema s) d))

(defmacro ignoring
  [expr]
  (if (:ns &env)
    `(try ~expr (catch :default e# nil))
    `(try ~expr (catch Throwable e# nil))))

(defn simple-schema
  [& {:keys [pred] :as options}]
  (cond-> options
    (contains? options :type-properties)
    (update :type-properties (fn [props]
                               (cond-> props
                                 (contains? props :decode/string)
                                 (update :decode/string (fn [decode-fn]
                                                          (fn [s]
                                                            (if (pred s)
                                                              s
                                                              (or (ignoring (decode-fn s)) s)))))
                                 (contains? props ::decode)
                                 (update ::decode (fn [decode-fn]
                                                    (fn [s]
                                                      (if (pred s)
                                                        s
                                                        (or (ignoring (decode-fn s)) s))))))))
    :always
    (m/-simple-schema)))

(defn lookup
  "Lookups schema from registry."
  ([s] (lookup sr/default-registry s))
  ([registry s] (schema (mr/schema registry s))))

(defn- get-assert-context
  [env form sname]
  (if-let [nsdata (:ns env)]
    {:ns (str (:name nsdata))
     :schema sname
     :line (:line env)
     :file (:file (:meta nsdata))}
    {:ns   (str (ns-name *ns*))
     :schema sname
     :line (:line (meta form))}))

(defmacro assert!
  [& [s value hint]]
  (when *assert*
    `(let [s# (schema ~s)
           v# ~value
           h# ~(or hint (str "schema assert: " (pr-str s)))
           c# ~(get-assert-context &env &form (pr-str s))]
       (if (validate s# v#)
         v#
         (let [exp#  (explain s# v#)
               data# {:type :assertion
                      :code :data-validation
                      :hint h#
                      ::explain exp#}]
           (throw (ex-info h# (into data# c#))))))))

(defmacro assert-fn
  [s]
  (if *assert*
    `(let [s#    (schema ~s)
           v-fn# (lazy-validator s#)
           e-fn# (lazy-explainer s#)]
       (fn [v#]
         (when-not (v-fn# v#)
           (let [hint# ~(str "schema assert: " (pr-str s))
                 exp#  (e-fn# v#)]
             (throw (ex-info hint#
                             {:type :assertion
                              :code :data-validation
                              :hint hint#
                              ::explain exp#}))))))
    `(constantly nil)))

(defmacro verify-fn
  [s]
  (binding [*assert* true]
    `(assert-fn ~s)))

(defmacro verify!
  "A variant of `assert!` macro that evaluates always, independently
  of the *assert* value."
  [& params]
  (binding [*assert* true]
    `(assert! ~@params)))

(defn register! [type s]
  (let [s (if (map? s) (simple-schema s) s)]
    (swap! sr/registry assoc type s)))

(defn def! [type s]
  (register! type s)
  nil)

;; --- GENERATORS

(defn gen-set-from-choices
  [choices]
  (->> tgen/nat
       (tgen/fmap (fn [i]
                    (into #{}
                          (map (fn [_] (rand-nth choices)))
                          (range i))))))


;; --- BUILTIN SCHEMAS

(def! :merge (mu/-merge))
(def! :union (mu/-union))

(def uuid-rx
  #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")

(def! ::uuid
  {:type ::uuid
   :pred uuid?
   :type-properties
   {:title "uuid"
    :description "UUID formatted string"
    :error/message "should be an uuid"
    :gen/gen (sg/uuid)
    ::oapi/type "string"
    ::oapi/format "uuid"
    ::decode #(uuid/uuid (re-matches uuid-rx %))}})

(def non-empty-strings-xf
  (comp
   (filter string?)
   (remove str/empty?)
   (remove str/blank?)))

(def! ::set-of-strings
  {:type ::set-of-strings
   :pred #(and (set? %) (every? string? %))
   :type-properties
   {:title "set[type=string]"
    :description "Set of Strings"
    :error/message "should be an set of strings"
    :gen/gen (-> :string sg/generator sg/set)
    ::oapi/type "array"
    ::oapi/format "set"
    ::oapi/items {:type "string"}
    ::oapi/unique-items true
    ::decode (fn [v]
               (into #{} non-empty-strings-xf (str/split v #"[\s,]+")))}})

(def! ::one-of
  {:type ::one-of
   :min 1
   :max 1
   :compile (fn [props children options]
              (let [options (into #{} (last children))
                    format  (:format props "keyword")]
                {:pred #(contains? options %)
                 :type-properties
                 {:title "one-of"
                  :description "One of the Set"
                  :gen/gen (sg/elements options)
                  ::oapi/type "string"
                  ::oapi/format (:format props "keyword")
                  ::decode (if (= format "keyword")
                             keyword
                             identity)}}))})

(def max-safe-int (int 1e6))
(def min-safe-int (int -1e6))

(def! ::safe-int
  {:type ::safe-int
   :pred #(and (int? %) (>= max-safe-int %) (>= % min-safe-int))
   :type-properties
   {:title "int"
    :description "Safe Integer"
    :error/message "expected to be int in safe range"
    :gen/gen (sg/small-int)
    ::oapi/type "integer"
    ::oapi/format "int64"
    ::oapi/decode parse-long}})

(def! ::safe-number
  {:type ::safe-number
   :pred #(and (number? %) (>= max-safe-int %) (>= % min-safe-int))
   :type-properties
   {:title "number"
    :description "Safe Number"
    :error/message "expected to be number in safe range"
    :gen/gen (sg/one-of (sg/small-int)
                        (sg/small-double))
    ::oapi/type "number"
    ::oapi/format "double"
    ::oapi/decode parse-double}})

(def! ::safe-double
  {:type ::safe-double
   :pred #(and (double? %) (>= max-safe-int %) (>= % min-safe-int))
   :type-properties
   {:title "number"
    :description "Safe Number"
    :error/message "expected to be number in safe range"
    :gen/gen (sg/small-double)
    ::oapi/type "number"
    ::oapi/format "double"
    ::oapi/decode parse-double}})

(def! ::contains-any
  {:type ::contains-any
   :min 1
   :max 1
   :compile (fn [props children options]
              (let [choices (last children)
                    pred    (if (:strict props)
                              #(some (fn [prop]
                                       (some? (get % prop)))
                                     choices)
                              #(some (fn [prop]
                                       (contains? % prop))
                                     choices))]
                {:pred pred
                 :type-properties
                 {:title "contains"
                  :description "contains predicate"}}))})
