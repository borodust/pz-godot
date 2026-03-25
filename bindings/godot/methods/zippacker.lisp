(common-lisp:in-package :%godot)


(defgmethod (zippacker+open :class 'zippacker :bind "open" :hash 1936816515)
 error (path string) (append zippacker+zip-append))

(defgmethod
 (zippacker+set-compression-level :class 'zippacker :bind
  "set_compression_level" :hash 1286410249)
 :void (compression-level int))

(defgmethod
 (zippacker+get-compression-level :class 'zippacker :bind
  "get_compression_level" :hash 3905245786)
 int)

(defgmethod
 (zippacker+start-file :class 'zippacker :bind "start_file" :hash 166001499)
 error (path string))

(defgmethod
 (zippacker+write-file :class 'zippacker :bind "write_file" :hash 680677267)
 error (data packed-byte-array))

(defgmethod
 (zippacker+close-file :class 'zippacker :bind "close_file" :hash 166280745)
 error)

(defgmethod (zippacker+close :class 'zippacker :bind "close" :hash 166280745)
 error)