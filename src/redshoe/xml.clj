(ns redshoe.xml)

; Conversion from XML structures to sequences

(defn- process-generic-tag
  [{ tag :tag content :content }]
  (when content
    [tag (first content)]))

(defn- process-item-tag
  [{ tag :tag content :content }]
  (when (= :item tag)
    (into {} (map process-generic-tag content))))

(defn- process-field-tag
  [{ tag :tag content :content }]
  (when (= :field tag)
    (into {} (map process-generic-tag content))))

(defn- process-records-tag
  [{ tag :tag content :content }]
  (when (= :records tag)
    (map process-item-tag content)))

(defn- process-arms-tag
  [{ tag :tag content :content }]
  (when (= :arms tag)
    (map process-item-tag content)))

(defn- process-events-tag
  [{ tag :tag content :content }]
  (when (= :events tag)
    (map process-item-tag content)))

(defn- process-items-tag
  [{ tag :tag content :content }]
  (when (= :items tag)
    (map process-item-tag content)))

(defn- process-fields-tag
  [{ tag :tag content :content }]
  (when (= :fields tag)
    (map process-field-tag content)))

(defn records->seq
  [doc]
  (process-records-tag doc))

(defn arms->seq
  [doc]
  (process-arms-tag doc))

(defn events->seq
  [doc]
  (process-events-tag doc))

(defn items->seq
  [doc]
  (process-items-tag doc))

(defn fields->seq
  [doc]
  (process-fields-tag doc))

; Conversion from sequences to XML structures

(defn- process-generic-pairs
  [[tag-name value]]
  {
   :tag (keyword tag-name)
   :attrs nil
   :content [value] })

(defn- process-item-map
  [m]
  {
   :tag :item
   :attrs nil
   :content (mapv process-generic-pairs m) })

(defn- process-field-map
  [m]
  {
   :tag :field
   :attrs nil
   :content (mapv process-generic-pairs m) })

(defn- process-records-seq
  [s]
  {
   :tag :records
   :attrs nil
   :content (mapv process-item-map s) })

(defn- process-arms-seq
  [s]
  {
   :tag :arms
   :attrs nil
   :content (mapv process-item-map s) })

(defn- process-events-seq
  [s]
  {
   :tag :events
   :attrs nil
   :content (mapv process-item-map s) })

(defn- process-items-seq
  [s]
  {
   :tag :items
   :attrs nil
   :content (mapv process-item-map s) })

(defn- process-fields-seq
  [s]
  {
   :tag :fields
   :attrs nil
   :content (mapv process-field-map s) })

(defn seq->records
  [s]
  (process-records-seq s))

(defn seq->arms
  [s]
  (process-arms-seq s))

(defn seq->events
  [s]
  (process-events-seq s))

(defn seq->items
  [s]
  (process-items-seq s))

(defn seq->fields
  [s]
  (process-fields-seq s))
