(ns short-hand.core
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io])
  (:import (java.io ByteArrayInputStream)))

(defn- to-long-name [short-name]
  ;todo
  short-name
  )

(defn to-long-hand [short-hand]
  ;todo
  nil)

(defn read-long-hand [^String xml-string]
  (-> xml-string
      .getBytes
      ByteArrayInputStream.
      xml/parse
      zip/xml-zip))

(defn write-long-hand [long-hand]
  (-> long-hand xml/emit-element with-out-str))

(defn spit-long-hand [file-name long-hand]
  (spit file-name (write-long-hand long-hand)))

(defn slurp-long-hand [file-name]
  (-> file-name
      io/file
      xml/parse
      zip/xml-zip))

(defn- to-short-name [long-name]
  ;todo
  long-name
  )

(defn to-short-hand [{:keys [tag attrs children]}]
  (let [tag (to-short-name tag)
        attrs (if (empty? attrs) [] [(reduce-kv #(assoc %1 (to-short-name %2) %3) {} attrs)])
        children (if (empty? children) [] children)
        attrs-and-children (concat attrs children)]
    (if (empty? attrs-and-children)
      tag
      (into [tag] attrs-and-children))))


(defn read-short-hand [xml-string]
  (-> xml-string
      read-long-hand
      to-short-hand))

(defn write-short-hand [short-hand]
  (-> short-hand
      to-long-hand
      write-long-hand))

(defn spit-short-hand [file-name short-hand]
  (spit-long-hand file-name (to-long-hand short-hand))
  )

(defn slurp-short-hand [file-name]
  (to-short-hand (slurp-long-hand file-name)))
