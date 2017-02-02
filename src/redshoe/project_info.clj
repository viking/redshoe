(ns redshoe.project-info
  (:require [clojure.string :as string]
            clj-time.format))

(def ^{:private true} date-formatter
  (clj-time.format/formatter-local "yyyy-MM-dd HH:mm:ss"))

(defn- str->boolean
  [s]
  (= s "1"))

(def ^{:private true} conversions
  {:creation_time (partial clj-time.format/parse-local date-formatter)
   :display_today_now_button str->boolean
   :is_longitudinal str->boolean
   :ddp_enabled str->boolean
   :randomization_enabled str->boolean
   :in_production str->boolean
   :scheduling_enabled str->boolean
   :surveys_enabled str->boolean
   :record_autonumbering_enabled str->boolean})

(defn process-project-info
  [info]
  (reduce (fn [m [k v]] (update m k v))
          info conversions))
