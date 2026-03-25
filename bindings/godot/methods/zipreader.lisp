(common-lisp:in-package :%godot)


(defgmethod (zipreader+open :class 'zipreader :bind "open" :hash 166001499)
 error (path string))

(defgmethod (zipreader+close :class 'zipreader :bind "close" :hash 166280745)
 error)

(defgmethod
 (zipreader+get-files :class 'zipreader :bind "get_files" :hash 2981934095)
 packed-string-array)

(defgmethod
 (zipreader+read-file :class 'zipreader :bind "read_file" :hash 740857591)
 packed-byte-array (path string) (case-sensitive bool))

(defgmethod
 (zipreader+file-exists :class 'zipreader :bind "file_exists" :hash 35364943)
 bool (path string) (case-sensitive bool))

(defgmethod
 (zipreader+get-compression-level :class 'zipreader :bind
  "get_compression_level" :hash 3694577386)
 int (path string) (case-sensitive bool))