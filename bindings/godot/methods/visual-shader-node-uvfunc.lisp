(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-uvfunc+set-function :class 'visual-shader-node-uvfunc
  :bind "set_function" :hash 765791915)
 :void (func visual-shader-node-uvfunc+function))

(defgmethod
 (visual-shader-node-uvfunc+get-function :class 'visual-shader-node-uvfunc
  :bind "get_function" :hash 3772902164)
 visual-shader-node-uvfunc+function)