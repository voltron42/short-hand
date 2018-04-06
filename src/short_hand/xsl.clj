(ns short-hand.xsl
  (import (javax.xml.transform.stream StreamSource StreamResult)
          (java.io Reader Writer)
          (javax.xml.transform TransformerFactory)))

(defn transform-xml
  [^Reader xml-in ^Reader xslt ^Writer xml-out]
  (.transform
    (.newTransformer
      (TransformerFactory/newInstance)
      (StreamSource. xslt))
    (StreamSource. xml-in)
    (StreamResult. xml-out)))
