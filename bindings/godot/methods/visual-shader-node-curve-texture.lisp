(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-curve-texture+set-texture :class
  'visual-shader-node-curve-texture :bind "set_texture" :hash 181872837)
 :void (texture curve-texture))

(defgmethod
 (visual-shader-node-curve-texture+get-texture :class
  'visual-shader-node-curve-texture :bind "get_texture" :hash 2800800579)
 curve-texture)