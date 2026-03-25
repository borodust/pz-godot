(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-particle-accelerator+set-mode :class
  'visual-shader-node-particle-accelerator :bind "set_mode" :hash 3457585749)
 :void (mode visual-shader-node-particle-accelerator+mode))

(defgmethod
 (visual-shader-node-particle-accelerator+get-mode :class
  'visual-shader-node-particle-accelerator :bind "get_mode" :hash 2660365633)
 visual-shader-node-particle-accelerator+mode)