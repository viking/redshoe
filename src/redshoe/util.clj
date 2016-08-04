(ns redshoe.util
  (:require [clojure.string :as string]
            [redshoe.http :as http]
            [redshoe.records :as records]))

(defn export-chunked-records
  "Fetch records from the REDCap API in chunks to mitigate REDCap API timeout
  issues"
  ([url token primary-key size]
    (when-let [doc (http/export-field-values url token primary-key)]
      (->> (records/from-xml doc)
           (map (keyword primary-key))
           (partition-all size)
           (export-chunked-records url token))))
  ([url token id-partitions]
   (if-not (empty? id-partitions)
     (let [ids (string/join "," (first id-partitions))]
       (lazy-cat
         (records/from-xml (http/export-records url token {:records ids}))
         (export-chunked-records url token (rest id-partitions)))))))
