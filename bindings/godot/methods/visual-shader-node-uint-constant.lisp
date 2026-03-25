(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-uint-constant+set-constant :class
  'visual-shader-node-uint-constant :bind "set_constant" :hash 1286410249)
 :void (constant int))

(defgmethod
 (visual-shader-node-uint-constant+get-constant :class
  'visual-shader-node-uint-constant :bind "get_constant" :hash 3905245786)
 int)