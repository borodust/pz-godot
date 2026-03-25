(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-3constant+set-constant :class
  'visual-shader-node-vec-3constant :bind "set_constant" :hash 3460891852)
 :void (constant vector-3))

(defgmethod
 (visual-shader-node-vec-3constant+get-constant :class
  'visual-shader-node-vec-3constant :bind "get_constant" :hash 3360562783)
 vector-3)