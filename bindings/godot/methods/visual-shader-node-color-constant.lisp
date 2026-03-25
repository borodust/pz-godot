(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-color-constant+set-constant :class
  'visual-shader-node-color-constant :bind "set_constant" :hash 2920490490)
 :void (constant color))

(defgmethod
 (visual-shader-node-color-constant+get-constant :class
  'visual-shader-node-color-constant :bind "get_constant" :hash 3444240500)
 color)