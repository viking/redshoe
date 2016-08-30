(ns redshoe.http
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]))

(defn- do-export
  [url token content options]
  (let [params (merge {:token token, :format "xml", :content content} options)
        opts { :form-params params, :as :stream, :throw-exceptions false }
        request (client/post url opts)]
    (if (= 200 (:status request))
      (xml/parse (:body request))
      (binding [*out* *err*]
        (println (slurp (:body request)))))))

(defn- do-import
  [url token content tags options]
  (let [data (with-out-str (xml/emit tags))
        defaults {:token token, :format "xml", :content content, :data data}
        params (merge defaults options)
        request (client/post url {:form-params params, :as :stream})]
    (when (= 200 (:status request))
      (xml/parse (:body request)))))

(defn export-fields
  "Fetch the data dictionary from the REDCap API"
  [url token]
  (do-export url token "metadata" {}))

(defn export-arms
  "Fetch event arms from the REDCap API in XML format and parse"
  ([url token]
   (export-arms url token {}))
  ([url token options]
   (do-export url token "arm" options)))

(defn export-events
  "Fetch events from the REDCap API in XML format and parse"
  ([url token]
   (export-events url token {}))
  ([url token options]
   (do-export url token "event" options)))

(defn export-mappings
  "Fetch instrument-event mappings from the REDCap API in XML format and parse"
  ([url token]
   (export-mappings url token {}))
  ([url token options]
   (do-export url token "formEventMapping" options)))

(defn export-field-names
  "Fetch field names from the REDCap API in XML format and parse"
  ([url token]
   (export-field-names url token {}))
  ([url token options]
   (do-export url token "exportFieldNames" options)))

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
