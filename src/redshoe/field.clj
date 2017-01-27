(ns redshoe.field
  (:require [clojure.string :as string]
            clj-time.format))

; Conversion helpers
(defn- str->datetime
  [fmt-str value]
  (->
    (clj-time.format/formatter-local fmt-str)
    (clj-time.format/parse-local value)))

(defn- str->time
  [fmt-str value]
  (->
    (clj-time.format/formatter-local fmt-str)
    (clj-time.format/parse-local-time value)))

(defn- str->boolean
  [value]
  (= value "y"))

(defn- str->number
  [value]
  (bigdec value))

(defn- str->integer
  [value]
  (Integer. value))

; Table for converting min/max validation metadata
(def ^{:private true} validation-converters
  (let [convert-date (partial str->datetime "yyyy-MM-dd")
        convert-datetime (partial str->datetime "yyyy-MM-dd HH:mm")
        convert-datetime-s (partial str->datetime "yyyy-MM-dd HH:mm:ss")]
  {"date_ymd" convert-date
   "date_mdy" convert-date
   "date_dmy" convert-date
   "time" (partial str->time "HH:mm")
   "datetime_ymd" convert-datetime
   "datetime_mdy" convert-datetime
   "datetime_dmy" convert-datetime
   "datetime_seconds_ymd" convert-datetime-s
   "datetime_seconds_mdy" convert-datetime-s
   "datetime_seconds_dmy" convert-datetime-s
   "number" str->number
   "integer" str->integer}))

(defn- process-validation
  [field]
  (let
    [{field-type :type
      validation-type :text_validation_type_or_show_slider_number
      validation-min :text_validation_min
      validation-max :text_validation_max
      choices :select_choices_or_calculations} field
    field (dissoc
            field
            :text_validation_type_or_show_slider_number
            :text_validation_min
            :text_validation_max
            :select_choices_or_calculations)]

    (case field-type
      "text"
      (let [converter (get validation-converters validation-type)]
        ; Convert min and max values from strings to the appropriate type
        (->>
          (filter second
                  {:type validation-type
                   :min (and converter validation-min (converter validation-min))
                   :max (and converter validation-max (converter validation-max))
                   })
          (into {})
          (assoc field :validation)))

      "slider"
      (->>
        (filter second
                {:show_slider_number validation-type
                 :slider_labels (string/split choices #"\s+\|\s+")})
        (into {})
        (merge field))

      ("dropdown" "radio" "checkbox")
      (->>
        (string/split choices #"\s*\|\s*")
        (mapv #(zipmap [:code :label] (string/split % #",\s+")))
        (assoc field :choices))

      "calc"
      (assoc field :formula choices)

      ; default
      field)))

; Table for renaming field keys
(def ^{:private true} field-key-changes
  {:field_type :type
   :field_name :name
   :field_label :label})

; Table for converting field value metadata
; NOTE: These take place after the field keys have changed
(def ^{:private true} field-value-conversions
  {:identifier str->boolean
   :required str->boolean})

(defn- convert-values
  [field]
  (reduce (fn [f [k v]]
            (if (contains? f k) (update f k v) f))
          field field-value-conversions))

(defn process-field
  [field]
  (->
    (clojure.set/rename-keys field field-key-changes)
    (convert-values)
    (process-validation)))

(defn try-process-field
  [field]
  (try
    (process-field field)
    (catch Exception e
      (prn field)
      (throw e))))
