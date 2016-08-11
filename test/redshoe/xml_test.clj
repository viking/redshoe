(ns redshoe.xml-test
  (:require [clojure.test :refer :all]
            [redshoe.xml :refer :all]
            [clojure.xml :as xml]))

(defn parse-xml-str
  [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes s))))

(deftest test-records->seq
  (testing "Converting XML records structure to a sequence"
    (let [s "<records><item><foo>bar</foo></item><item><baz>qux</baz></item></records>"
          data (parse-xml-str s)]
      (is (=
           '({ :foo "bar" } { :baz "qux" })
           (records->seq data))))))

(deftest test-seq->records
  (testing "Converting a sequence to an XML records structure"
    (let [s "<records><item><foo>bar</foo></item><item><baz>qux</baz></item></records>"
          data (parse-xml-str s)]
      (is (=
           data
           (seq->records '({ :foo "bar" } { :baz "qux" })))))))
