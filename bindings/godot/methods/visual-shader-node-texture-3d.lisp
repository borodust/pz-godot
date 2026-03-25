(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-texture-3d+set-texture :class
  'visual-shader-node-texture-3d :bind "set_texture" :hash 1188404210)
 :void (value texture-3d))

(defgmethod
 (visual-shader-node-texture-3d+get-texture :class
  'visual-shader-node-texture-3d :bind "get_texture" :hash 373985333)
 texture-3d)