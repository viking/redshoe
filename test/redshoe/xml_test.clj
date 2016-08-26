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

(deftest test-arms->seq
  (testing "Converting XML arms structure to a sequence"
    (let [s "<arms><item><foo>bar</foo></item><item><baz>qux</baz></item></arms>"
          data (parse-xml-str s)]
      (is (=
           '({ :foo "bar" } { :baz "qux" })
           (arms->seq data))))))

(deftest test-seq->arms
  (testing "Converting a sequence to an XML arms structure"
    (let [s "<arms><item><foo>bar</foo></item><item><baz>qux</baz></item></arms>"
          data (parse-xml-str s)]
      (is (=
           data
           (seq->arms '({ :foo "bar" } { :baz "qux" })))))))

(deftest test-events->seq
  (testing "Converting XML events structure to a sequence"
    (let [s "<events><item><foo>bar</foo></item><item><baz>qux</baz></item></events>"
          data (parse-xml-str s)]
      (is (=
           '({ :foo "bar" } { :baz "qux" })
           (events->seq data))))))

(deftest test-seq->events
  (testing "Converting a sequence to an XML events structure"
    (let [s "<events><item><foo>bar</foo></item><item><baz>qux</baz></item></events>"
          data (parse-xml-str s)]
      (is (=
           data
           (seq->events '({ :foo "bar" } { :baz "qux" })))))))

(deftest test-items->seq
  (testing "Converting XML items structure to a sequence"
    (let [s "<items><item><foo>bar</foo></item><item><baz>qux</baz></item></items>"
          data (parse-xml-str s)]
      (is (=
           '({ :foo "bar" } { :baz "qux" })
           (items->seq data))))))

(deftest test-seq->items
  (testing "Converting a sequence to an XML items structure"
    (let [s "<items><item><foo>bar</foo></item><item><baz>qux</baz></item></items>"
          data (parse-xml-str s)]
      (is (=
           data
           (seq->items '({ :foo "bar" } { :baz "qux" })))))))

(deftest test-fields->seq
  (testing "Converting XML fields structure to a sequence"
    (let [s "<fields><field><foo>bar</foo></field><field><baz>qux</baz></field></fields>"
          data (parse-xml-str s)]
      (is (=
           '({ :foo "bar" } { :baz "qux" })
           (fields->seq data))))))

(deftest test-seq->fields
  (testing "Converting a sequence to an XML fields structure"
    (let [s "<fields><field><foo>bar</foo></field><field><baz>qux</baz></field></fields>"
          data (parse-xml-str s)]
      (is (=
           data
           (seq->fields '({ :foo "bar" } { :baz "qux" })))))))
