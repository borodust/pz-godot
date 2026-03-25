(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-transform-func+set-function :class
  'visual-shader-node-transform-func :bind "set_function" :hash 2900990409)
 :void (func visual-shader-node-transform-func+function))

(defgmethod
 (visual-shader-node-transform-func+get-function :class
  'visual-shader-node-transform-func :bind "get_function" :hash 2839926569)
 visual-shader-node-transform-func+function)