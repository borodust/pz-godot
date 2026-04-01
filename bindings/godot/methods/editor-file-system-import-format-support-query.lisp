(common-lisp:in-package :%godot)


(defgmethod
 (editor-file-system-import-format-support-query+%is-active :class
  'editor-file-system-import-format-support-query :bind "_is_active" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-file-system-import-format-support-query+%get-file-extensions :class
  'editor-file-system-import-format-support-query :bind "_get_file_extensions"
  :hash 1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-file-system-import-format-support-query+%query :class
  'editor-file-system-import-format-support-query :bind "_query" :hash 36873697
  :virtual common-lisp:t)
 bool)