(import
  '(java.security MessageDigest)
  '(java.io FileInputStream))

(defn create-checksum [^String filename]
  (let [input-stream (FileInputStream. filename)
        complete (MessageDigest/getInstance "MD5")
        buffer (byte-array 1024)]
    (loop [num-read (.read input-stream buffer)]
      (when-not (= num-read -1)
        (do
          (when (> num-read 0)
            (.update complete buffer 0 num-read))
          (recur (.read input-stream buffer)))))
    (.close input-stream)
    (.digest complete)))

(defn md5sum [filename]
  (let [checksum-bytes (create-checksum filename)
        buffer (StringBuffer. "")]
    (doseq [ch checksum-bytes]
      (.append buffer
        (subs
          (Integer/toString
            (bit-or (bit-and ch 0xff) 0x100)
            16)
          1)))
    (.toString buffer)))