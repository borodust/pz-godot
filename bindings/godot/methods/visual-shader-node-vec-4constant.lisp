(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-4constant+set-constant :class
  'visual-shader-node-vec-4constant :bind "set_constant" :hash 1727505552)
 :void (constant quaternion))

(defgmethod
 (visual-shader-node-vec-4constant+get-constant :class
  'visual-shader-node-vec-4constant :bind "get_constant" :hash 1222331677)
 quaternion)