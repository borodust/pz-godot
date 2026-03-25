(common-lisp:in-package :%godot)


(defgmethod
 (resource-importer+-get-build-dependencies :class 'resource-importer :bind
  "_get_build_dependencies" :hash 4291131558 :virtual common-lisp:t)
 packed-string-array (path string))