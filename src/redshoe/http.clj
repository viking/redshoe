(ns redshoe.http
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]))

(defn- export
  [url token content]
  (let [params {:token token, :format "xml", :content content}
        request (client/post url {:form-params params, :as :stream})]
    (when (= 200 (:status request))
      (xml/parse (:body request)))))

(defn export-dictionary
  "Fetch the data dictionary from the REDCap API"
  [url token]
  (export url token "metadata"))
