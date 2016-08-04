(ns redshoe.records)

(defn- process-item-content
  [{ tag :tag content :content }]
  (when content
    [tag (first content)]))

(defn- process-item
  [{ tag :tag content :content }]
  (when (= :item tag)
    (into (sorted-map) (map process-item-content content))))

(defn- process-records
  [{ tag :tag content :content }]
  (when (= :records tag)
    (map process-item content)))

(defn from-xml
  [doc]
  (process-records doc))
