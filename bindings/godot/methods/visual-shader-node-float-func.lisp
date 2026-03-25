(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-float-func+set-function :class
  'visual-shader-node-float-func :bind "set_function" :hash 536026177)
 :void (func visual-shader-node-float-func+function))

(defgmethod
 (visual-shader-node-float-func+get-function :class
  'visual-shader-node-float-func :bind "get_function" :hash 2033948868)
 visual-shader-node-float-func+function)