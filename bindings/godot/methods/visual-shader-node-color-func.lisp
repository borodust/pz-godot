(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-color-func+set-function :class
  'visual-shader-node-color-func :bind "set_function" :hash 3973396138)
 :void (func visual-shader-node-color-func+function))

(defgmethod
 (visual-shader-node-color-func+get-function :class
  'visual-shader-node-color-func :bind "get_function" :hash 554863321)
 visual-shader-node-color-func+function)