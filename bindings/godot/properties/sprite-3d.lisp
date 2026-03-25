(common-lisp:in-package :%godot)


(defgproperty sprite-3d+texture 'sprite-3d :get 'sprite-3d+get-texture :set
 'sprite-3d+set-texture)

(defgproperty sprite-3d+hframes 'sprite-3d :get 'sprite-3d+get-hframes :set
 'sprite-3d+set-hframes)

(defgproperty sprite-3d+vframes 'sprite-3d :get 'sprite-3d+get-vframes :set
 'sprite-3d+set-vframes)

(defgproperty sprite-3d+frame 'sprite-3d :get 'sprite-3d+get-frame :set
 'sprite-3d+set-frame)

(defgproperty sprite-3d+frame-coords 'sprite-3d :get
 'sprite-3d+get-frame-coords :set 'sprite-3d+set-frame-coords)

(defgproperty sprite-3d+region-enabled 'sprite-3d :get
 'sprite-3d+is-region-enabled :set 'sprite-3d+set-region-enabled)

(defgproperty sprite-3d+region-rect 'sprite-3d :get 'sprite-3d+get-region-rect
 :set 'sprite-3d+set-region-rect)