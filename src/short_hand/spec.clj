(ns short-hand.spec
  (:require [clojure.spec.alpha :as s]
            [pred-i-kit.core :as p]))

(def short-name #"[a-zA-Z][a-zA-Z0-9_-]*")

(s/def :short/name
  (s/and symbol?
         (s/or :with-ns (p/named-as short-name short-name)
               :no-ns (p/named-as short-name))))

(s/def :short/primitive
  (s/or :number number?
        :string string?
        :boolean boolean?))

(s/def :short/attrs (s/map-of :short/name :short/primitive))

(s/def :short/node
  (s/or :single :short/name
        :full (s/and vector?
                     (s/cat :name :short/name
                            :attrs (s/? :short/attrs)
                            :children (s/* (s/or :node :short/node
                                                 :primitive :short/primitive))))))

(def long-name #"[a-zA-Z][a-zA-Z0-9_-]*([:][a-zA-Z][a-zA-Z0-9_-]*)?")

(s/def :long/tag (s/and keyword? long-name))

(s/def :long/attrs (s/map-of :long/tag string?))

(s/def :long/children
  (s/and vector?
         (s/or :node :long/node
               :string string?)))

(s/def :long/node (s/keys :req-un [:long/tag]
                          :opt-un [:long/attrs :long/children]))

