(common-lisp:in-package :%godot)


(defgmethod
 (pckpacker+pck-start :class 'pckpacker :bind "pck_start" :hash 508410629)
 error (pck-path string) (alignment int) (key string) (encrypt-directory bool))

(defgmethod
 (pckpacker+add-file :class 'pckpacker :bind "add_file" :hash 2215643711) error
 (target-path string) (source-path string) (encrypt bool))

(defgmethod
 (pckpacker+add-file-from-buffer :class 'pckpacker :bind "add_file_from_buffer"
  :hash 1131482346)
 error (target-path string) (data packed-byte-array) (encrypt bool))

(defgmethod
 (pckpacker+add-file-removal :class 'pckpacker :bind "add_file_removal" :hash
  166001499)
 error (target-path string))

(defgmethod (pckpacker+flush :class 'pckpacker :bind "flush" :hash 1633102583)
 error (verbose bool))