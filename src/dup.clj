(load "file_utils")
(load "md5")

(defn parse [id-fn filename]
  (let [key (id-fn filename)]
    {key [filename]}))

(def parse-by-md5
  (partial parse md5sum))

(def parse-by-name
  (partial parse
    (fn [f] (.getName (File. ^String f)))))

; for examining effectiveness of threading
(def parse-slowly
  (partial parse
    (fn [f] (Thread/sleep 10) f)))

(def ^:dynamic *id-algo*)

(defn identify-and-merge
  [all-files id-algo]
  (let [agents (map agent all-files)]
    (doseq [agent agents] (send-off agent id-algo))
    (apply await agents)
    (let [results (doall (pmap deref agents))]
      (reduce (partial merge-with concat) results))))

(defn find-dups [start-dir]
  (let [todo-dirs (transient [start-dir])
        todo-files (transient [])]
    (loop []
      (if-let [next-todo (nth todo-dirs (-> todo-dirs count dec) nil)]
        (let [file-entries (list-of-files next-todo)]
          (pop! todo-dirs)
          (when-not (empty? file-entries)
            (letfn [(conj-entries! [entries pred]
                      (reduce conj! entries (filter pred file-entries)))]
              (conj-entries! todo-dirs is-directory?)
              (conj-entries! todo-files is-file?)))
          (recur))
        (identify-and-merge (persistent! todo-files) *id-algo*)))))

(defn go [start-dir]
  (binding [*id-algo* parse-by-name]
    (println "Starting...")
    (doseq [dup
            (filter
              #(> (count (val %)) 1)
              (find-dups start-dir))]
      (println dup))))
