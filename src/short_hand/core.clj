(ns short-hand.core
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [short-hand.spec :as spec]
            [clojure.spec.alpha :as s])
  (:import (java.io ByteArrayInputStream InputStream OutputStream BufferedWriter OutputStreamWriter ByteArrayOutputStream)
           (clojure.lang ExceptionInfo)))

(defn- validate [my-spec body]
  (when-let [error (s/explain-data my-spec body)]
    (throw (ExceptionInfo. "Invalid content" {:error error})))
  body)

(def ^:private xml-entities
  {\< "&lt;"
   \> "&gt;"
   \& "&amp;"
   \' "&apos;"
   \" "&quot;"})

(defn- escape [str-val]
  (str/escape str-val xml-entities))

(defn- unescape [str-val]
  (reduce-kv
    #(str/replace %1 (re-pattern %3) (str %2))
    str-val
    xml-entities))

(defn- to-long-name [short-name]
  (let [my-namespace (namespace short-name)
        my-name (name short-name)]
    (if-not (nil? my-namespace)
      (keyword (str my-namespace ":" my-name))
      (keyword my-name))))

(defn to-long-hand [short-hand]
  (let [short-hand (validate ::spec/short-node short-hand)
        body (if (symbol? short-hand) [short-hand] short-hand)
        [tag attrs & content] body
        tag (to-long-name tag)
        attrs (if (nil? attrs) {} attrs)
        [content attrs] (if (map? attrs)
                          [content (reduce (fn [out [k v]] (assoc out (to-long-name k) (escape (str v)))) {} attrs)]
                          [(into [attrs] content) {}])
        content (if-not (empty? content)
                  (mapv #(if (or (vector? %) (symbol? %)) (to-long-hand %) (str %)) content)
                  content)
        node (reduce-kv #(if (empty? %3) %1 (assoc %1 %2 %3))
                        {:tag tag}
                        {:attrs attrs :content content})]
    (validate ::spec/long-node node)))

(defn- trim-parsing [{:keys [tag attrs content]}]
  (let [content (if-not (empty? content)
                  (mapv #(if (string? %) (str/trim %) (trim-parsing %)) content)
                  content)
        node (reduce-kv #(if (empty? %3) %1 (assoc %1 %2 %3))
                        {:tag tag}
                        {:attrs attrs :content content})]
    node))

(defn stream-long-hand [^InputStream stream]
  (->> stream
      xml/parse
      trim-parsing
      (validate ::spec/long-node)))

(defn long-hand-to-stream [long-hand ^OutputStream stream]
  (let [validated (validate ::spec/long-node long-hand)]
    (with-open [w (BufferedWriter. (OutputStreamWriter. stream))]
      (binding [*out* w]
        (xml/emit-element validated)))))

(defn read-long-hand [^String xml-string]
  (-> xml-string
      .getBytes
      ByteArrayInputStream.
      stream-long-hand))

(defn write-long-hand [long-hand]
  (let [out-str (ByteArrayOutputStream.)]
    (long-hand-to-stream long-hand out-str)
    (.toString out-str)))

(defn spit-long-hand [file-name long-hand]
  (let [out-str (io/output-stream file-name)]
    (long-hand-to-stream long-hand out-str)
    (.toString out-str)))

(defn slurp-long-hand [file-name]
  (-> file-name
      io/input-stream
      stream-long-hand))

(defn- to-short-name [long-name]
  (let [[my-ns my-name] (str/split (name long-name) #":")
        [my-ns my-name] (if-not (nil? my-name) [my-ns my-name] [nil my-ns])]
    (symbol my-ns my-name)))

(defn to-short-hand [long-hand]
  (let [{:keys [tag attrs content]} (validate ::spec/long-node long-hand)
        tag (to-short-name tag)
        attrs (if (empty? attrs) [] [(reduce-kv #(assoc %1 (to-short-name %2) (unescape %3)) {} attrs)])
        content (if (empty? content) [] (mapv #(if (string? %) % (to-short-hand %)) content))
        attrs-and-content (concat attrs content)
        body (if (empty? attrs-and-content) tag (into [tag] attrs-and-content))]
    (validate ::spec/short-node body)))

(defn read-short-hand [xml-string]
  (-> xml-string
      read-long-hand
      to-short-hand))

(defn write-short-hand [short-hand]
  (-> short-hand
      to-long-hand
      write-long-hand))

(defn spit-short-hand [file-name short-hand]
  (spit-long-hand file-name (to-long-hand short-hand)))

(defn slurp-short-hand [file-name]
  (to-short-hand (slurp-long-hand file-name)))
