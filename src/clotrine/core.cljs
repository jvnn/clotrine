(ns clotrine.core
  (:require [clotrine.logger :as log]
            [clotrine.parser :as parser]
            [cljs.nodejs :as node]
            [clojure.string :as s]))

(def md-it (node/require "markdown-it"))
(def md (md-it.))
(def sanitizer (node/require "sanitizer"))

(defn init
  "Initialize the rendering engine. Options is a map that includes:
    dirs: [...]
    debug: boolean"
  [opts]
  (assert (some? (:dirs opts)))
  (when (true? (:debug opts)) (log/set-debug-mode))
  (when (true? (:silent opts)) (log/set-silent-mode))
  (let [data (parser/parse-templates (:dirs opts))
        style-hash (.createHash (node/require "crypto") "md5")]
    (.update style-hash (:style data))
    (assoc data :style-hash (.digest style-hash "hex"))))

(defn- expand-variables-in-string [src vars]
  (reduce #(s/replace %1 (first %2) (vars (second %2))) src (re-seq #"\$\{([a-zA-Z0-9_-]+)\}" src)))

(defn- expand-variables "Expand any variables given as '$variablename' in the source data."
  [src vars]
  (cond
    (map? src)
    (reduce #(assoc %1 %2 (if (string? (src %2))
                            (expand-variables-in-string (src %2) vars)
                            (src %2)))
            {}
            (keys src))
    (string? src) 
    (expand-variables-in-string src vars)
    :default
    src))

(defn render [write-fn templates name data]
  (let [render-array (templates name)]
    (log/debug (str "Starting to render template " name " with data " data))
    (if (nil? render-array)
      (log/warn (str "Trying to render a non-existing template " name))
      (loop [current (first render-array)
             remaining (rest render-array)
             skipping nil]
        (when (some? current)
          (if (some? skipping)
            ; skipping means we've earlier encountered an if that evaluated into false
            (if (and (#{:ifelse :endif} (:type current)) (= (:data current) skipping))
              (recur (first remaining) (rest remaining) nil) ; matching else or end-if found -> stop skipping
              (recur (first remaining) (rest remaining) skipping))

            ; standard case (not skipping):
            (case (:type current)
              :raw (do
                     (write-fn (:data current))
                     (recur (first remaining) (rest remaining) nil))

              ; fill -> write whatever data was given with the name from the template
              :fill (do
                      (write-fn (.escape sanitizer (or (data (:data current)) "")))
                      (recur (first remaining) (rest remaining) nil))

              ; anon-fill -> write the whole data object (assuming it's a string)
              :anon-fill (do
                           (if (string? data)
                             (write-fn (.escape sanitizer data))
                             (log/warn "Non-string data given to a template with anonymous fill"))
                           (recur (first remaining) (rest remaining) nil))

              ; markdown -> like a fill, but run the data through the markdown converter
              :markdown (do
                          (write-fn (.render md (or (data (:data current)) "")))
                          (recur (first remaining) (rest remaining) nil))

              ; include -> run render on the component given in the include tag
              ; note: the data must be a vector to support looping includes
              :include (let [alias (:alias (:data current))
                             hard-coded-data (:hard-coded (:data current))
                             is-hard-coded (some? hard-coded-data)
                             subdata (if is-hard-coded
                                       (map #(expand-variables % data) hard-coded-data)
                                       (data alias))
                             component (:component (:data current))]
                         (doseq [d subdata]
                           (render write-fn templates component d))
                         (recur (first remaining) (rest remaining) nil))

              ; if -> if the data map value for the if name resolves into true,
              ; start skipping using the name of the if. If an :ifvalue field exists,
              ; compare the data map value to this value and skip if they don't match.
              :if (let [name-of-if (:data current)
                        value-from-data (data name-of-if)
                        ifvalue (:ifvalue current)
                        should-skip (or (nil? value-from-data)
                                        (and (coll? value-from-data) (empty? value-from-data))
                                        (and (some? ifvalue) (not= value-from-data ifvalue))
                                        (false? value-from-data))
                        skipping-value (if should-skip name-of-if nil)]
                    (recur (first remaining) (rest remaining) skipping-value))

              ; else and not skipping -> the contents of the else branch should be skipped
              :ifelse (recur (first remaining) (rest remaining) (:data current))

              ; not skipping, endif can be ignored
              :endif (recur (first remaining) (rest remaining) nil))))))
    (log/debug (str "... done rendering " name))))
