(common-lisp:in-package :%godot)


(defgmethod
 (box-shape-3d+set-size :class 'box-shape-3d :bind "set_size" :hash 3460891852)
 :void (size vector-3))

(defgmethod
 (box-shape-3d+get-size :class 'box-shape-3d :bind "get_size" :hash 3360562783)
 vector-3)