(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-transform-constant+set-constant :class
  'visual-shader-node-transform-constant :bind "set_constant" :hash 2952846383)
 :void (constant transform-3d))

(defgmethod
 (visual-shader-node-transform-constant+get-constant :class
  'visual-shader-node-transform-constant :bind "get_constant" :hash 3229777777)
 transform-3d)