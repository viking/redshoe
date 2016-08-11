(ns redshoe.http
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]))

(defn- do-export
  [url token content options]
  (let [params (merge {:token token, :format "xml", :content content} options)
        request (client/post url {:form-params params, :as :stream})]
    (when (= 200 (:status request))
      (xml/parse (:body request)))))

(defn- do-import
  [url token content tags options]
  (let [data (with-out-str (xml/emit tags))
        defaults {:token token, :format "xml", :content content, :data data}
        params (merge defaults options)
        request (client/post url {:form-params params, :as :stream})]
    (when (= 200 (:status request))
      (xml/parse (:body request)))))

(defn export-dictionary
  "Fetch the data dictionary from the REDCap API"
  [url token]
  (do-export url token "metadata" {}))

(defn export-records
  "Fetch records from the REDCap API in XML format and parse"
  ([url token]
   (export-records url token {}))
  ([url token options]
   (let [opts (merge { :type "flat" } options)]
     (do-export url token "record" opts))))

(defn export-field-values
  "Fetch single field value from each record using from the REDCap API, for
  example, the primary key field"
  [url token field-name]
  (export-records url token { :fields field-name }))

(defn import-records
  "Import records to the REDCap API"
  ([url token tags]
   (import-records url token tags {}))
  ([url token tags options]
   (let [opts (merge { :type "flat" } options)]
     (do-import url token "record" tags opts))))
