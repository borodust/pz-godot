(common-lisp:in-package :%godot)


(defgmethod
 (editor-translation-parser-plugin+%parse-file :class
  'editor-translation-parser-plugin :bind "_parse_file" :hash 1576865988
  :virtual common-lisp:t)
 array (path string))

(defgmethod
 (editor-translation-parser-plugin+%get-recognized-extensions :class
  'editor-translation-parser-plugin :bind "_get_recognized_extensions" :hash
  1139954409 :virtual common-lisp:t)
 packed-string-array)