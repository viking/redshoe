(ns redshoe.dictionary)

(defn- process-xml-item-content
  [{ tag :tag content :content }]
  (when content
    [tag (first content)]))

(defn- process-xml-item
  [{ tag :tag content :content }]
  (when (= :item tag)
    (into {} (map process-xml-item-content content))))

(defn- process-xml-records
  [{ tag :tag content :content }]
  (when (= :records tag)
    (map process-xml-item content)))

(defn from-xml
  [doc]
  (process-xml-records doc))
