(common-lisp:in-package :%godot)


(defgmethod
 (point-light-2d+set-texture :class 'point-light-2d :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (point-light-2d+get-texture :class 'point-light-2d :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (point-light-2d+set-texture-offset :class 'point-light-2d :bind
  "set_texture_offset" :hash 743155724)
 :void (texture-offset vector-2))

(defgmethod
 (point-light-2d+get-texture-offset :class 'point-light-2d :bind
  "get_texture_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (point-light-2d+set-texture-scale :class 'point-light-2d :bind
  "set_texture_scale" :hash 373806689)
 :void (texture-scale float))

(defgmethod
 (point-light-2d+get-texture-scale :class 'point-light-2d :bind
  "get_texture_scale" :hash 1740695150)
 float)