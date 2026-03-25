(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-is+set-function :class 'visual-shader-node-is :bind
  "set_function" :hash 1438374690)
 :void (func visual-shader-node-is+function))

(defgmethod
 (visual-shader-node-is+get-function :class 'visual-shader-node-is :bind
  "get_function" :hash 580678557)
 visual-shader-node-is+function)