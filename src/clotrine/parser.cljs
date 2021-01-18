(ns clotrine.parser
  (:require [clotrine.logger :as log]
            [clojure.string :as s]
            [cljs.nodejs :as node]))

(def fs (node/require "fs"))
(def path (node/require "path"))
(def md-it (node/require "markdown-it"))
(def md (md-it.))

(def SEPARATOR #"(\{\{|\}\}|::|\|\|)")
; clojurescript on node can't handle splitting with groups in the regex... >:(
(def SEP_TO_SPLIT {"{{" #"\{\{"
                   "::" #"::"
                   "||" #"\|\|"
                   "}}" #"\}\}"})

(defn- make-class-prefix [name]
  (s/replace name "/" "-"))

(defn- replace-single-classname [name prefix]
  ; allow special fields (e.g. fills) in classnames -> check that global names
  ; only have one @-sign
  (if (re-find #"^@[^@]+" name)
    (subs name 1)
    (str prefix "-" name)))

(defn- get-class-replacement [old prefix]
  (let [class-vec (s/split old #" ")]
    (str "class=\"" (s/join " " (map #(replace-single-classname % prefix) class-vec)) "\"")))

(defn- read-md-file [filename]
  (.render md (.readFileSync fs filename "utf8")))

(defn- get-next-step [remaining]
  (if-let [next-separator (re-find SEPARATOR remaining)]
    (let [sep (first next-separator)
          parts (s/split remaining (SEP_TO_SPLIT sep) 2)]
      (assoc {}
             :before (first parts)
             :after (second parts)
             :type (cond
                     (= sep "{{") :opening
                     (= sep "}}") :closing
                     (= sep "::") :ifbranch
                     (= sep "||") :elsebranch
                     :else :invalid)))

    ; if we don't find any more separators, this will be the last raw block
    (assoc {}
           :before remaining
           :after nil
           :type nil)))

; required to use re-matches with possible newlines and trailing spaces
(defn- clean-include-parameter [param]
  (s/replace (s/trim param) "\n" ""))

(defn parse-html [db-root name-path orig-contents]
  (log/debug (str "parsing html " name-path))
  (let [class-prefix (make-class-prefix name-path)
        ; the horror below replaces all individual classnames within all class definitions
        ; E.G. class="one @global two" becomes class="prefix-one global prefix-two"
        contents (loop [to-replace (re-seq #"class=\"([^\"]+)\"" orig-contents)
                        new-contents orig-contents]
                   (let [next (first to-replace)]
                     (if (empty? next)
                       new-contents
                       (recur (rest to-replace)
                              (s/replace-first new-contents
                                               (first next)
                                               (get-class-replacement (second next) class-prefix))))))
        first-separator (s/index-of contents SEPARATOR)]
    (loop [context (get-next-step contents)
           open-ifs (list)
           render-array []]
      (if (nil? (:after context))
        (conj render-array {:type :raw :data (:before context)})
        (cond
          (and (seq open-ifs) (= (last (:before context)) "?") (= (:type context) :closing))
          ; we've found an end to an if
          (recur
            (get-next-step (:after context))
            (rest open-ifs)
            (conj render-array
                  {:type :raw :data (s/join (butlast (:before context)))}
                  {:type :endif :data (first open-ifs)}))

          (and (seq open-ifs) (= (:type context) :elsebranch))
          ; we've found an else (call it "ifelse" though to avoid collisions with the :else keyword)
          (do
            (assert (not= (:type (last render-array)) :ifelse) "Cannot have two consecutive else directives")
            (recur
              (get-next-step (:after context))
              open-ifs
              (conj render-array
                    {:type :raw :data (:before context)}
                    {:type :ifelse :data (first open-ifs)})))

          (= (:type context) :opening)
          ; new identifier after some raw data
          (recur
            (get-next-step (:after context))
            open-ifs
            (conj render-array {:type :raw :data (:before context)}))

          (= (first (:before context)) "#")
          ; we've found an include, check if an alias or hard-coded data (as JSON array) is given
          (let [include-parts (s/split (subs (:before context) 1) #" " 2)
                parameter (if (second include-parts) (clean-include-parameter (second include-parts)) nil)
                alias (if (some? parameter) parameter (first include-parts))
                hard-coded-data (if (and (some? parameter) (re-matches #"^\[.*\]$" parameter)) (js->clj (.parse js/JSON parameter)) nil)
                component (first include-parts)]
            (recur
              (get-next-step (:after context))
              open-ifs
              (conj render-array {:type :include :data {:alias alias :component component :hard-coded hard-coded-data}})))

          (= (first (:before context)) "?")
          ; we've found an if
          (do
            (assert (= (:type context) :ifbranch) (str "Unexpected control identifier '" (:type context) "' after if command"))
            (if-let [fields (re-find #"\?([^=]+)==([^=]+)" (:before context))]
              ; it's an if with value (variable==value)
              (let [ifname (second fields)
                    ifvalue (nth fields 2)]
                (recur
                  (get-next-step (:after context))
                  (conj open-ifs ifname)
                  (conj render-array {:type :if :data ifname :ifvalue ifvalue})))
              (let [ifname (subs (:before context) 1)]
                (recur
                  (get-next-step (:after context))
                  (conj open-ifs ifname)
                  (conj render-array {:type :if :data ifname})))))

          (= (first (:before context)) "!")
          ; we've found a markdown field
          (recur
            (get-next-step (:after context))
            open-ifs
            (conj render-array {:type :markdown :data (subs (:before context) 1)}))

          (= (first (:before context)) "=")
          ; static markdown from a file
          (recur
            (get-next-step (:after context))
            open-ifs
            (conj render-array {:type :raw :data (read-md-file (.join path db-root (subs (:before context) 1)))}))

          :else
          ; must be a fill
          (if (= "_" (:before context))
            ; it's an anonymous fill: the whole data will be inserted here and no other types are allowed in this template
            (recur
              (get-next-step (:after context))
              open-ifs
              (conj render-array {:type :anon-fill}))
            (recur
              (get-next-step (:after context))
              open-ifs
              (conj render-array {:type :fill :data (:before context)}))))))))

(defn- validate-parsed-html [name data]
  (let [has-anon-fill (some #(= (:type %) :anon-fill) data)
        has-other-non-raw-types (some #(and (not= (:type %) :anon-fill) (not= (:type %) :raw)) data)]
    (assert (not (and has-anon-fill has-other-non-raw-types)) (str "Error when parsing " name ": if an anonymous fill is used, all other types of tags are forbidden"))))

; avoid replacing anything inside quotes
(defn- replace-class-names-in-line [line class-prefix]
  (let [parts (s/split line #"\"")
        not-in-quotes (take-nth 2 parts)
        in-quotes (take-nth 2 (rest parts))
        ; disallow numbers right after the dot to avoid replacing something like "border: solid 1.5em black;"
        replaced (map #(s/replace % #"\.([a-zA-Z]{1}[a-zA-Z0-9-]*)" (str "." class-prefix "-$1")) not-in-quotes)
        merged (if (> (count not-in-quotes) (count in-quotes))
                 (conj (vec (interleave replaced in-quotes)) (last replaced))
                 (interleave replaced in-quotes))]
    (s/join "\"" merged)))


(defn- parse-css [name-path contents]
  (log/debug (str "parsing css " name-path))
  (let [class-prefix (make-class-prefix name-path)
        lines (s/split contents #"\n")
        replaced (map #(replace-class-names-in-line % class-prefix) lines)
        joined (str (s/join "\n" replaced) "\n")]
    ; in the end replace all classnames with ".@whatever" with ".whatever" to allow names that should not be modified
    (s/replace joined #"\.@([a-zA-Z]+)" (str "." "$1"))))

(defn- split-definitions [string-def]
  (let [varname (re-find #"^[^=]+" string-def)
        value (second (re-find #"=(.*)" string-def))]
    [varname value]))

(defn- parse-definitions [file-contents]
  (log/debug "parsing definitions file")
  (let [raw-lines (s/split file-contents #"\n")
        lines (filter #(when (and (not (s/blank? %)) (not= (first %) ";")) %) raw-lines)]
    (assert (some? (reduce #(and %1 (re-matches #"^[a-zA-Z0-9-_]+=.*" %2)) true lines)) "Invalid definitions file")
    (reduce #(assoc %1 (first %2) (second %2)) {} (map split-definitions lines))))

(defn- parse-file [db-root dir filename data]
  (let [full-name (.join path db-root dir filename)
        name-path (s/replace (.join path dir filename) #"\.[a-zA-Z]+$" "")]
    (cond
      (s/ends-with? full-name ".html")
      (let [parsed-html (parse-html db-root name-path (.readFileSync fs full-name "utf8"))]
        (validate-parsed-html full-name parsed-html)
        (assoc data name-path parsed-html))
      ; definitions should be put to a file called "definitions.ini" in the root
      (and (s/blank? dir) (= filename "definitions.ini"))
      (assoc data :definitions (parse-definitions (.readFileSync fs full-name "utf8")))
      ; global styles live in the root dir and must have this name
      (and (s/blank? dir) (= filename "global.css"))
      (assoc data :global-style (.readFileSync fs full-name "utf8"))
      (s/ends-with? full-name ".css")
      (assoc data :style (str (:style data) (parse-css name-path
                                                       (.readFileSync fs full-name "utf8"))))
      ; in case of unrecogniced files, return the existing data to avoid losing it...
      :else data)))

(defn- get-stat [db-root dir file]
  (.statSync fs (.join path db-root dir file)))

(defn- merge-data [map1 map2]
  (let [style1 (:style map1)
        style2 (:style map2)
        global1 (:global-style map1)
        global2 (:global-style map2)]
    (assoc (merge map1 map2) :style (str style1 style2) :global-style (str global1 global2))))

(defn- parse-dir [db-root cur-dir contents data]
  (let [subdirs (filter #(.isDirectory (get-stat db-root cur-dir %)) contents)
        files (filter #(.isFile (get-stat db-root cur-dir %)) contents)]
    (merge-data data
      (merge-data
        (loop [subdir (first subdirs)
               remaining (rest subdirs)
               subdata {}]
          (if (empty? subdir)
            subdata
            (recur
              (first remaining)
              (rest remaining)
              (parse-dir db-root
                         (.join path cur-dir subdir)
                         (.readdirSync fs (.join path db-root cur-dir subdir))
                         subdata))))
        (loop [file (first files)
               remaining (rest files)
               subdata {}]
          (if (empty? file)
            subdata
            (recur
              (first remaining)
              (rest remaining)
              (parse-file db-root cur-dir file subdata))))))))

(defn replace-definition [to-replace data]
  ((:definitions data) (subs to-replace 1)))

(defn parse-templates [directories]
  (loop [data {}
         dirs directories]
    (if (nil? (first dirs))
      ; done, perform some finalizations
      (let [style-merged (if (contains? data :global-style)
                             ; global style must always be the first due to imports
                             (dissoc (assoc data :style (str (:global-style data) (:style data))) :global-style)
                             data)]
        ; replace definitions
        (if (contains? style-merged :definitions)
          (let [old-style (:style style-merged)
                style-with-defs (s/replace old-style #"\$[a-zA-Z0-9-_]+" #(replace-definition % style-merged))]
            (dissoc (assoc style-merged :style style-with-defs) :definitions))
          style-merged))
      (recur (parse-dir (first dirs) "" (.readdirSync fs (first dirs)) data) (rest dirs)))))
