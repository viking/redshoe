(ns redshoe.util
  (:require [clojure.string :as string]
            [redshoe.http :as http]
            [redshoe.xml :as xml]))

(defn export-dictionary
  [url token]
  (xml/records->seq (http/export-dictionary url token)))

(defn export-arms
  [url token]
  (xml/arms->seq (http/export-arms url token)))

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
         (xml/records->seq (http/export-records url token {:records ids}))
         (export-chunked-records url token (rest id-partitions)))))))
