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
 (zippacker+add-directory :class 'zippacker :bind "add_directory" :hash
  934773537)
 error (path string) (permissions file-access+unix-permission-flags)
 (modified-time int))

(defgmethod
 (zippacker+start-file :class 'zippacker :bind "start_file" :hash 4260848715)
 error (path string) (permissions file-access+unix-permission-flags)
 (modified-time int))

(defgmethod
 (zippacker+write-file :class 'zippacker :bind "write_file" :hash 680677267)
 error (data packed-byte-array))

(defgmethod
 (zippacker+close-file :class 'zippacker :bind "close_file" :hash 166280745)
 error)

(defgmethod (zippacker+close :class 'zippacker :bind "close" :hash 166280745)
 error)