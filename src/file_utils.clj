(import '(java.io File))

(defn kind [^String filename]
  (let [f (File. filename)]
    (cond
      (.isFile f)       :file
      (.isDirectory f)  :directory
      (.exists f)       :other
      :else             :non-existent )))

(defn is-file? [filename]
  (= :file (kind filename)))

(defn is-directory? [filename]
  (= :directory (kind filename)))

(defn list-of-files [^String directory]
  (map #(.getAbsolutePath ^File %) (.listFiles (File. directory))))