(common-lisp:in-package :%godot)


(defgproperty point-light-2d+texture 'point-light-2d :get
 'point-light-2d+get-texture :set 'point-light-2d+set-texture)

(defgproperty point-light-2d+offset 'point-light-2d :get
 'point-light-2d+get-texture-offset :set 'point-light-2d+set-texture-offset)

(defgproperty point-light-2d+texture-scale 'point-light-2d :get
 'point-light-2d+get-texture-scale :set 'point-light-2d+set-texture-scale)

(defgproperty point-light-2d+height 'point-light-2d)