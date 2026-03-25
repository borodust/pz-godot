(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-boolean-constant+set-constant :class
  'visual-shader-node-boolean-constant :bind "set_constant" :hash 2586408642)
 :void (constant bool))

(defgmethod
 (visual-shader-node-boolean-constant+get-constant :class
  'visual-shader-node-boolean-constant :bind "get_constant" :hash 36873697)
 bool)