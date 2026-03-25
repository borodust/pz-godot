(common-lisp:in-package :%godot)


(defgmethod
 (gdextension+is-library-open :class 'gdextension :bind "is_library_open" :hash
  36873697)
 bool)

(defgmethod
 (gdextension+get-minimum-library-initialization-level :class 'gdextension
  :bind "get_minimum_library_initialization_level" :hash 964858755)
 gdextension+initialization-level)