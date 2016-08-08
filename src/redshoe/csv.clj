(ns redshoe.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn write-csv
  "Writes a CSV file with the given columns and records. The `records`
  parameter should be a collection of maps. The `columns` parameter should be a
  collection of keys that coincide with the keys in `records`. Values in
  `columns` will be converted using the `name` function before outputting."
  [filename columns records]
  (with-open [out-file (io/writer filename)]
    (let [header (map name columns)
          rows (map #(map % columns) records)]
      (csv/write-csv out-file (cons header rows)))))
