(ns redshoe.util
  (:require [clojure.string :as string]
            [redshoe.http :as http]
            [redshoe.xml :as xml]
            clj-time.format))

; Helper composition functions
(def export-arms (comp xml/arms->seq http/export-arms))
(def export-events (comp xml/events->seq http/export-events))
(def export-mappings (comp xml/items->seq http/export-mappings))
(def export-records (comp xml/records->seq http/export-records))
(def export-field-names (comp xml/fields->seq http/export-field-names))

(defn- parse-datetime
  [fmt-str value]
  (->
    (clj-time.format/formatter-local fmt-str)
    (clj-time.format/parse-local value)))

(def ^{:private true} validation-coercions
  {"date_ymd" (partial parse-datetime "yyyy-MM-dd")
   "date_mdy" (partial parse-datetime "MM-dd-yyyy")
   "date_dmy" (partial parse-datetime "dd-MM-yyyy")
   "time" (partial parse-datetime "HH:mm")
   "datetime_ymd" (partial parse-datetime "yyyy-MM-dd HH:mm")
   "datetime_mdy" (partial parse-datetime "MM-dd-yyyy HH:mm")
   "datetime_dmy" (partial parse-datetime "dd-MM-yyyy HH:mm")
   "datetime_seconds_ymd" (partial parse-datetime "yyyy-MM-dd HH:mm:ss")
   "datetime_seconds_mdy" (partial parse-datetime "MM-dd-yyyy HH:mm:ss")
   "datetime_seconds_dmy" (partial parse-datetime "dd-MM-yyyy HH:mm:ss")
   "number" bigdec
   "integer" #(Integer. %)})

(defn- process-field
  [field]
  (let [{ftype :field_type
         vtype :text_validation_type_or_show_slider_number
         vmin :text_validation_min
         vmax :text_validation_max
         choices :select_choices_or_calculations
         identifier :identifier} field
        field (->
                (if (contains? field :identifier)
                  (assoc field :identifier (= identifier "y"))
                  field)
                (dissoc
                  :text_validation_type_or_show_slider_number
                  :text_validation_min
                  :text_validation_max
                  :select_choices_or_calculations))]

    (case ftype
      "text"
      (let [f (get validation-coercions vtype)]
        (->>
          (filter second
                  {:type vtype
                   :min (and f vmin (f vmin))
                   :max (and f vmax (f vmax))
                   })
          (into {})
          (assoc field :validation)))

      "slider"
      (->>
        (filter second
                {:show_slider_number vtype
                 :slider_labels (string/split choices #"\s+\|\s+")})
        (into {})
        (merge field))

      ("dropdown" "radio" "checkbox")
      (->>
        (string/split choices #"\s+\|\s+")
        (mapv #(zipmap [:code :label] (string/split % #",\s+")))
        (assoc field :choices))

      "calc"
      (assoc field :formula choices)

      ; default
      field)))

(defn export-fields
  "Export field metadata from the REDCap API and process"
  [url token]
  (->>
    (http/export-fields url token)
    (xml/records->seq)
    (map process-field)))

(defn- add-exports-to-field
  [field-name-groups field]
  (assoc field :exports (get field-name-groups (:field_name field))))

(defn- organize-fields
  [fields field-name-groups]
  (map (partial add-exports-to-field field-name-groups) fields))

(defn- add-forms-to-event
  [mapping-groups field-groups event]
  (->>
    (get mapping-groups (:unique_event_name event))
    (map :form)
    (select-keys field-groups)
    (assoc event :forms)))

(defn- organize-events
  [events mapping-groups field-groups]
  (map (partial add-forms-to-event mapping-groups field-groups) events))

(defn- add-events-to-arm
  [event-groups arm]
  (assoc arm :events (get event-groups (:arm_num arm))))

(defn- organize-arms
  [arms event-groups]
  (map (partial add-events-to-arm event-groups) arms))

(defn export-arms-tree
  "Fetch field metadata and organize by arm and event"
  [url token]
  (let [field-names (export-field-names url token)
        fields (export-fields url token)
        mappings (export-mappings url token)
        events (export-events url token)
        arms (export-arms url token)]
    (->>
      (organize-fields fields (group-by :original_field_name field-names))
      (group-by :form_name)
      (organize-events events (group-by :unique_event_name mappings))
      (group-by :arm_num)
      (organize-arms arms))))

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
