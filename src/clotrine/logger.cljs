(ns clotrine.logger)

(def is-debug (atom false))
(def is-silent (atom false))

(defn- write-log [level text]
  (when (not @is-silent)
    (println (str (.toISOString (js/Date.)) ": [" level "] " text))))

(defn debug [text]
  (when @is-debug (write-log " DEBUG " text)))

(defn warn [text]
  (write-log "WARNING" text))

(defn set-debug-mode []
  (reset! is-debug true))

(defn set-silent-mode []
  (reset! is-silent true))
