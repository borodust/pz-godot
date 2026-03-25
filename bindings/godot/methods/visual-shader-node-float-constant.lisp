(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-float-constant+set-constant :class
  'visual-shader-node-float-constant :bind "set_constant" :hash 373806689)
 :void (constant float))

(defgmethod
 (visual-shader-node-float-constant+get-constant :class
  'visual-shader-node-float-constant :bind "get_constant" :hash 1740695150)
 float)