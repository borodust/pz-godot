(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-uint-func+set-function :class
  'visual-shader-node-uint-func :bind "set_function" :hash 2273148961)
 :void (func visual-shader-node-uint-func+function))

(defgmethod
 (visual-shader-node-uint-func+get-function :class
  'visual-shader-node-uint-func :bind "get_function" :hash 4187123296)
 visual-shader-node-uint-func+function)