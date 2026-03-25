(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-int-func+set-function :class 'visual-shader-node-int-func
  :bind "set_function" :hash 424195284)
 :void (func visual-shader-node-int-func+function))

(defgmethod
 (visual-shader-node-int-func+get-function :class 'visual-shader-node-int-func
  :bind "get_function" :hash 2753496911)
 visual-shader-node-int-func+function)