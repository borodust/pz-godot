(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vector-func+set-function :class
  'visual-shader-node-vector-func :bind "set_function" :hash 629964457)
 :void (func visual-shader-node-vector-func+function))

(defgmethod
 (visual-shader-node-vector-func+get-function :class
  'visual-shader-node-vector-func :bind "get_function" :hash 4047776843)
 visual-shader-node-vector-func+function)