(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-particle-emit+set-flags :class
  'visual-shader-node-particle-emit :bind "set_flags" :hash 3960756792)
 :void (flags visual-shader-node-particle-emit+emit-flags))

(defgmethod
 (visual-shader-node-particle-emit+get-flags :class
  'visual-shader-node-particle-emit :bind "get_flags" :hash 171277835)
 visual-shader-node-particle-emit+emit-flags)