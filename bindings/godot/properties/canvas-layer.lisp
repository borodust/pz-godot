(common-lisp:in-package :%godot)


(defgproperty canvas-layer+layer 'canvas-layer :get 'canvas-layer+get-layer
 :set 'canvas-layer+set-layer)

(defgproperty canvas-layer+visible 'canvas-layer :get 'canvas-layer+is-visible
 :set 'canvas-layer+set-visible)

(defgproperty canvas-layer+offset 'canvas-layer :get 'canvas-layer+get-offset
 :set 'canvas-layer+set-offset)

(defgproperty canvas-layer+rotation 'canvas-layer :get
 'canvas-layer+get-rotation :set 'canvas-layer+set-rotation)

(defgproperty canvas-layer+scale 'canvas-layer :get 'canvas-layer+get-scale
 :set 'canvas-layer+set-scale)

(defgproperty canvas-layer+transform 'canvas-layer :get
 'canvas-layer+get-transform :set 'canvas-layer+set-transform)

(defgproperty canvas-layer+custom-viewport 'canvas-layer :get
 'canvas-layer+get-custom-viewport :set 'canvas-layer+set-custom-viewport)

(defgproperty canvas-layer+follow-viewport-enabled 'canvas-layer :get
 'canvas-layer+is-following-viewport :set 'canvas-layer+set-follow-viewport)

(defgproperty canvas-layer+follow-viewport-scale 'canvas-layer :get
 'canvas-layer+get-follow-viewport-scale :set
 'canvas-layer+set-follow-viewport-scale)