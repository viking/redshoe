(ns redshoe.util
  (:require [clojure.string :as string]
            [redshoe.http :as http]
            [redshoe.xml :as xml]))

; Helper composition functions
(def export-dictionary (comp xml/records->seq http/export-dictionary))
(def export-arms (comp xml/arms->seq http/export-arms))
(def export-events (comp xml/events->seq http/export-events))
(def export-mappings (comp xml/items->seq http/export-mappings))
(def export-records (comp xml/records->seq http/export-records))

(defn export-chunked-records
  "Fetch records from the REDCap API in chunks to mitigate REDCap API timeout
  issues"
  ([url token primary-key size]
    (when-let [doc (http/export-field-values url token primary-key)]
      (->> (xml/records->seq doc)
           (map (keyword primary-key))
           (partition-all size)
           (export-chunked-records url token))))
  ([url token id-partitions]
   (if-not (empty? id-partitions)
     (let [ids (string/join "," (first id-partitions))]
       (lazy-cat
         (export-records url token {:records ids})
         (export-chunked-records url token (rest id-partitions)))))))
