(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-2constant+set-constant :class
  'visual-shader-node-vec-2constant :bind "set_constant" :hash 743155724)
 :void (constant vector-2))

(defgmethod
 (visual-shader-node-vec-2constant+get-constant :class
  'visual-shader-node-vec-2constant :bind "get_constant" :hash 3341600327)
 vector-2)