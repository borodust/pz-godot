(common-lisp:in-package :%godot)


(defgmethod
 (rectangle-shape-2d+set-size :class 'rectangle-shape-2d :bind "set_size" :hash
  743155724)
 :void (size vector-2))

(defgmethod
 (rectangle-shape-2d+get-size :class 'rectangle-shape-2d :bind "get_size" :hash
  3341600327)
 vector-2)