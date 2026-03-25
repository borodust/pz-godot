(common-lisp:in-package :%godot)


(defgmethod
 (grid-container+set-columns :class 'grid-container :bind "set_columns" :hash
  1286410249)
 :void (columns int))

(defgmethod
 (grid-container+get-columns :class 'grid-container :bind "get_columns" :hash
  3905245786)
 int)