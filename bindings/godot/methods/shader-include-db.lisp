(common-lisp:in-package :%godot)


(defgmethod
 (shader-include-db+list-built-in-include-files :class 'shader-include-db :bind
  "list_built_in_include_files" :hash 2981934095 :static common-lisp:t)
 packed-string-array)

(defgmethod
 (shader-include-db+has-built-in-include-file :class 'shader-include-db :bind
  "has_built_in_include_file" :hash 2323990056 :static common-lisp:t)
 bool (filename string))

(defgmethod
 (shader-include-db+get-built-in-include-file :class 'shader-include-db :bind
  "get_built_in_include_file" :hash 1703090593 :static common-lisp:t)
 string (filename string))