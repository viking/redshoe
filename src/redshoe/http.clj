(ns redshoe.http
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]))

(defn- export
  [url token content options]
  (let [params (merge {:token token, :format "xml", :content content} options)
        request (client/post url {:form-params params, :as :stream})]
    (when (= 200 (:status request))
      (xml/parse (:body request)))))

(defn export-dictionary
  "Fetch the data dictionary from the REDCap API"
  [url token]
  (export url token "metadata" {}))

(defn export-records
  "Fetch records from the REDCap API"
  ([url token]
   (export-records url token {}))
  ([url token options]
   (let [opts (merge { :type "flat" } options)]
     (export url token "record" opts))))

(defn export-field-values
  "Fetch single field value from each record using from the REDCap API, for
  example, the primary key field"
  [url token field-name]
  (export-records url token { :fields field-name }))
