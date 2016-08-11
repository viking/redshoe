(ns redshoe.xml)

(defn- process-field-tag
  [{ tag :tag content :content }]
  (when content
    [tag (first content)]))

(defn- process-item-tag
  [{ tag :tag content :content }]
  (when (= :item tag)
    (into {} (map process-field-tag content))))

(defn- process-records-tag
  [{ tag :tag content :content }]
  (when (= :records tag)
    (map process-item-tag content)))

(defn tags->seq
  [doc]
  (process-records-tag doc))

(defn- process-field-pairs
  [[field-name value]]
  {
   :tag (keyword field-name)
   :attrs nil
   :content [value] })

(defn- process-item-map
  [m]
  {
   :tag :item
   :attrs nil
   :content (mapv process-field-pairs m) })

(defn- process-records-seq
  [s]
  {
   :tag :records
   :attrs nil
   :content (mapv process-item-map s) })

(defn seq->tags
  [s]
  (process-records-seq s))

