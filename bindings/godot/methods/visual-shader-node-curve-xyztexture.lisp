(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-curve-xyztexture+set-texture :class
  'visual-shader-node-curve-xyztexture :bind "set_texture" :hash 8031783)
 :void (texture curve-xyztexture))

(defgmethod
 (visual-shader-node-curve-xyztexture+get-texture :class
  'visual-shader-node-curve-xyztexture :bind "get_texture" :hash 1950275015)
 curve-xyztexture)