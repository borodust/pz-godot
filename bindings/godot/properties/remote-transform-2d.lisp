(common-lisp:in-package :%godot)


(defgproperty remote-transform-2d+remote-path 'remote-transform-2d :get
 'remote-transform-2d+get-remote-node :set 'remote-transform-2d+set-remote-node)

(defgproperty remote-transform-2d+use-global-coordinates 'remote-transform-2d
 :get 'remote-transform-2d+get-use-global-coordinates :set
 'remote-transform-2d+set-use-global-coordinates)

(defgproperty remote-transform-2d+update-position 'remote-transform-2d :get
 'remote-transform-2d+get-update-position :set
 'remote-transform-2d+set-update-position)

(defgproperty remote-transform-2d+update-rotation 'remote-transform-2d :get
 'remote-transform-2d+get-update-rotation :set
 'remote-transform-2d+set-update-rotation)

(defgproperty remote-transform-2d+update-scale 'remote-transform-2d :get
 'remote-transform-2d+get-update-scale :set
 'remote-transform-2d+set-update-scale)