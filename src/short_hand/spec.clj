(ns short-hand.spec
  (:require [clojure.spec.alpha :as s]
            [pred-i-kit.core :as p])
  (:import (java.util Formatter$DateTime)
           (org.joda.time DateTime))
  (:gen-class))

(defmulti coercible (fn [] nil))

(defmethod coercible :default []
  [])

(defn- is-coercible? []
  (fn [value]
    (not (empty? (filter #(instance? % value) (coercible))))))

(s/def ::short-name
  (s/and symbol?
         (s/or :with-ns (p/named-as #"[a-zA-Z][a-zA-Z0-9_-]*" #"[a-zA-Z][a-zA-Z0-9_-]*")
               :no-ns (p/named-as #"[a-zA-Z][a-zA-Z0-9_-]*"))))

(s/def ::short-primitive
  (s/or :number number?
        :string string?
        :boolean boolean?
        :simple-object (is-coercible?)))

(s/def ::short-attrs (s/map-of ::short-name ::short-primitive))

(s/def ::short-node
  (s/or :single ::short-name
        :full (s/and vector?
                     (s/cat :tag ::short-name
                            :attrs (s/? ::short-attrs)
                            :content (s/* (s/or :node ::short-node
                                                 :primitive ::short-primitive))))))

(s/def ::tag (s/and keyword? (p/named-as #"[a-zA-Z][a-zA-Z0-9_-]*([:][a-zA-Z][a-zA-Z0-9_-]*)?")))

(s/def ::attrs (s/map-of ::tag string?))

(s/def ::content (s/and vector?
                        (s/coll-of
                          (s/or :node ::long-node
                                :string string?))))

(s/def ::long-node (s/keys :req-un [::tag]
                           :opt-un [::attrs ::content]))

