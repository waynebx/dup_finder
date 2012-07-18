(load "file_utils")
(load "md5")

(def ^:dynamic *dup-algo*)
(def ^:dynamic *dup-agent*)

(defn parse [dups filename id-fn]
  (let [key (id-fn filename)
        currents (dups key)]
    (merge dups {key (conj currents filename)})))

(defn parse-by-md5 [dups filename]
  (parse dups filename md5sum))

(defn parse-by-name [dups filename]
  (parse dups filename
    (fn [^String f]
      (.getName (File. f)))))

(defn find-dup [file-entries to-dos dup-agent dup-algorithm]
  (loop [entries file-entries
         todos to-dos]
    (if (empty? entries)
      todos
      (let [entry (first entries)]
        (if (is-file? entry)
          (do
            (send-off dup-agent dup-algorithm entry)
            (recur (rest entries) todos))
          (do
            (recur (rest entries)
              (if (is-directory? entry)
                (conj (vec todos) entry)
                todos))))))))

(defn find-dups [start-dir dup-agent dup-algo]
  (loop [todos [start-dir]]
    (when-not (empty? todos)
      (let [file-entries (list-of-files (first todos))]
        (if (empty? file-entries)
          (recur (rest todos))
          (recur (rest (find-dup file-entries todos dup-agent dup-algo))))))))

(defn go []
  (binding [*dup-algo* parse-by-name
            *dup-agent* (agent {})]
    (println "Starting...")
    (find-dups "/home/gnt/tmp" *dup-agent* *dup-algo*)
    (await *dup-agent*)
    (let [dups (filter #(> (count (val %)) 1) @*dup-agent*)]
      (println dups))))