(common-lisp:in-package :%godot)


(defgmethod
 (cone-twist-joint-3d+set-param :class 'cone-twist-joint-3d :bind "set_param"
  :hash 1062470226)
 :void (param cone-twist-joint-3d+param) (value float))

(defgmethod
 (cone-twist-joint-3d+get-param :class 'cone-twist-joint-3d :bind "get_param"
  :hash 2928790850)
 float (param cone-twist-joint-3d+param))