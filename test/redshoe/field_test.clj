(ns redshoe.field-test
  (:require [clojure.test :refer :all]
            [redshoe.field :refer :all]
            [clj-time.core :as t]))

(deftest renaming-keys
  (is (= (process-field {:field_type "foo"}) {:type "foo"}))
  (is (= (process-field {:field_name "foo"}) {:name "foo"}))
  (is (= (process-field {:field_label "foo"}) {:label "foo"})))

(deftest converting-values
  (is (= (process-field {:identifier "y"}) {:identifier true}))
  (is (= (process-field {:identifier "n"}) {:identifier false}))
  (is (= (process-field {:identifier ""}) {:identifier false}))
  (is (= (process-field {:identifier "junk"}) {:identifier false}))
  (is (= (process-field {:required "y"}) {:required true}))
  (is (= (process-field {:required "n"}) {:required false}))
  (is (= (process-field {:required ""}) {:required false}))
  (is (= (process-field {:required "junk"}) {:required false})))

(def ^{:private true} text-validation-types
  {:number {:text_validation_type_or_show_slider_number "number"}
   :integer {:text_validation_type_or_show_slider_number "integer"}
   :date-ymd {:text_validation_type_or_show_slider_number "date_ymd"}
   :date-dmy {:text_validation_type_or_show_slider_number "date_dmy"}
   :date-mdy {:text_validation_type_or_show_slider_number "date_mdy"}
   :datetime-ymd {:text_validation_type_or_show_slider_number "datetime_ymd"}
   :datetime-dmy {:text_validation_type_or_show_slider_number "datetime_dmy"}
   :datetime-mdy {:text_validation_type_or_show_slider_number "datetime_mdy"}
   :datetime-seconds-ymd {:text_validation_type_or_show_slider_number "datetime_seconds_ymd"}
   :datetime-seconds-dmy {:text_validation_type_or_show_slider_number "datetime_seconds_dmy"}
   :datetime-seconds-mdy {:text_validation_type_or_show_slider_number "datetime_seconds_mdy"}
   :time {:text_validation_type_or_show_slider_number "time"}})

(deftest processing-validation
  (testing "text field type"
    (let [field {:field_type "text"}]
      (testing "number validation type"
        (let [field (merge field (:number text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "number"}}))
          (is (= (process-field (merge field {:text_validation_min "1.234"}))
              {:type "text" :validation {:type "number" :min 1.234M}}))
          (is (= (process-field (merge field {:text_validation_max "1.234"}))
              {:type "text" :validation {:type "number" :max 1.234M}}))
          (is (= (process-field
                (merge field {:text_validation_min "1.234" :text_validation_max "2.345"}))
              {:type "text" :validation {:type "number" :min 1.234M :max 2.345M}}))))

      (testing "integer validation type"
        (let [field (merge field (:integer text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "integer"}}))
          (is (= (process-field (merge field {:text_validation_min "1234"}))
              {:type "text" :validation {:type "integer" :min 1234}}))
          (is (= (process-field (merge field {:text_validation_max "1234"}))
              {:type "text" :validation {:type "integer" :max 1234}}))
          (is (= (process-field
                (merge field {:text_validation_min "1234" :text_validation_max "2345"}))
              {:type "text" :validation {:type "integer" :min 1234 :max 2345}}))))

      (testing "date_ymd validation type"
        (let [field (merge field (:date-ymd text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "date_ymd"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"}))
              {:type "text" :validation {:type "date_ymd" :min (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12"}))
              {:type "text" :validation {:type "date_ymd" :max (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"
                                              :text_validation_max "2099-01-12"}))
                 {:type "text"
                  :validation {:type "date_ymd"
                               :min (t/local-date-time 2000 1 12)
                               :max (t/local-date-time 2099 1 12)}}))))

      (testing "date_dmy validation type"
        (let [field (merge field (:date-dmy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "date_dmy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"}))
              {:type "text" :validation {:type "date_dmy" :min (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12"}))
              {:type "text" :validation {:type "date_dmy" :max (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"
                                              :text_validation_max "2099-01-12"}))
                 {:type "text"
                  :validation {:type "date_dmy"
                               :min (t/local-date-time 2000 1 12)
                               :max (t/local-date-time 2099 1 12)}}))))

      (testing "date_mdy validation type"
        (let [field (merge field (:date-mdy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "date_mdy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"}))
              {:type "text" :validation {:type "date_mdy" :min (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12"}))
              {:type "text" :validation {:type "date_mdy" :max (t/local-date-time 2000 1 12)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12"
                                              :text_validation_max "2099-01-12"}))
                 {:type "text"
                  :validation {:type "date_mdy"
                               :min (t/local-date-time 2000 1 12)
                               :max (t/local-date-time 2099 1 12)}}))))

      (testing "datetime_ymd validation type"
        (let [field (merge field (:datetime-ymd text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_ymd"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_ymd" :min (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_ymd" :max (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"
                                              :text_validation_max "2099-01-12 23:59"}))
                 {:type "text"
                  :validation {:type "datetime_ymd"
                               :min (t/local-date-time 2000 1 12 23 59)
                               :max (t/local-date-time 2099 1 12 23 59)}}))))

      (testing "datetime_dmy validation type"
        (let [field (merge field (:datetime-dmy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_dmy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_dmy" :min (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_dmy" :max (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"
                                              :text_validation_max "2099-01-12 23:59"}))
                 {:type "text"
                  :validation {:type "datetime_dmy"
                               :min (t/local-date-time 2000 1 12 23 59)
                               :max (t/local-date-time 2099 1 12 23 59)}}))))

      (testing "datetime_mdy validation type"
        (let [field (merge field (:datetime-mdy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_mdy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_mdy" :min (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59"}))
              {:type "text" :validation {:type "datetime_mdy" :max (t/local-date-time 2000 1 12 23 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59"
                                              :text_validation_max "2099-01-12 23:59"}))
                 {:type "text"
                  :validation {:type "datetime_mdy"
                               :min (t/local-date-time 2000 1 12 23 59)
                               :max (t/local-date-time 2099 1 12 23 59)}}))))

      (testing "datetime_seconds_ymd validation type"
        (let [field (merge field (:datetime-seconds-ymd text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_seconds_ymd"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_ymd" :min (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_ymd" :max (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"
                                              :text_validation_max "2099-01-12 23:59:59"}))
                 {:type "text"
                  :validation {:type "datetime_seconds_ymd"
                               :min (t/local-date-time 2000 1 12 23 59 59)
                               :max (t/local-date-time 2099 1 12 23 59 59)}}))))

      (testing "datetime_seconds_dmy validation type"
        (let [field (merge field (:datetime-seconds-dmy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_seconds_dmy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_dmy" :min (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_dmy" :max (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"
                                              :text_validation_max "2099-01-12 23:59:59"}))
                 {:type "text"
                  :validation {:type "datetime_seconds_dmy"
                               :min (t/local-date-time 2000 1 12 23 59 59)
                               :max (t/local-date-time 2099 1 12 23 59 59)}}))))

      (testing "datetime_seconds_mdy validation type"
        (let [field (merge field (:datetime-seconds-mdy text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "datetime_seconds_mdy"}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_mdy" :min (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_max "2000-01-12 23:59:59"}))
              {:type "text" :validation {:type "datetime_seconds_mdy" :max (t/local-date-time 2000 1 12 23 59 59)}}))
          (is (= (process-field (merge field {:text_validation_min "2000-01-12 23:59:59"
                                              :text_validation_max "2099-01-12 23:59:59"}))
                 {:type "text"
                  :validation {:type "datetime_seconds_mdy"
                               :min (t/local-date-time 2000 1 12 23 59 59)
                               :max (t/local-date-time 2099 1 12 23 59 59)}}))))

      (testing "time validation type"
        (let [field (merge field (:time text-validation-types))]
          (is (= (process-field field)
              {:type "text" :validation {:type "time"}}))
          (is (= (process-field (merge field {:text_validation_min "23:59"}))
              {:type "text" :validation {:type "time" :min (t/local-time 23 59)}}))
          (is (= (process-field (merge field {:text_validation_max "23:59"}))
              {:type "text" :validation {:type "time" :max (t/local-time 23 59)}}))
          (is (= (process-field (merge field {:text_validation_min "23:59"
                                              :text_validation_max "23:59"}))
                 {:type "text"
                  :validation {:type "time"
                               :min (t/local-time 23 59)
                               :max (t/local-time 23 59)}}))))))

  (testing "slider field type"
    (let [field {:field_type "slider"}]
      (testing "show slider number"
        (let [field (merge field {:text_validation_type_or_show_slider_number "number"})]
          (is (= (process-field field) {:type "slider" :show_slider_number true}))))
      (testing "slider labels"
        (let [field (merge field {:select_choices_or_calculations "foo | bar | baz"})]
          (is (= (process-field field) {:type "slider" :slider_labels ["foo" "bar" "baz"]}))))))

  (testing "dropdown field type"
    (let [field {:field_type "dropdown"}]
      (testing "choices"
        (let [field (merge field {:select_choices_or_calculations "0, foo | A, bar | z, baz"})]
          (is (= (process-field field)
                 {:type "dropdown"
                  :choices [{:code "0" :label "foo"}
                            {:code "A" :label "bar"}
                            {:code "z" :label "baz"}]}))))))

  (testing "radio field type"
    (let [field {:field_type "radio"}]
      (testing "choices"
        (let [field (merge field {:select_choices_or_calculations "0, foo | A, bar | z, baz"})]
          (is (= (process-field field)
                 {:type "radio"
                  :choices [{:code "0" :label "foo"}
                            {:code "A" :label "bar"}
                            {:code "z" :label "baz"}]}))))))

  (testing "checkbox field type"
    (let [field {:field_type "checkbox"}]
      (testing "choices"
        (let [field (merge field {:select_choices_or_calculations "0, foo | A, bar | z, baz"})]
          (is (= (process-field field)
                 {:type "checkbox"
                  :choices [{:code "0" :label "foo"}
                            {:code "A" :label "bar"}
                            {:code "z" :label "baz"}]}))))))

  (testing "calc field type"
    (let [field {:field_type "calc"}]
      (testing "formula"
        (let [field (merge field {:select_choices_or_calculations "foo bar"})]
          (is (= (process-field field) {:type "calc" :formula "foo bar"})))))))
