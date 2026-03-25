(common-lisp:in-package :%godot)


(defgproperty remote-transform-3d+remote-path 'remote-transform-3d :get
 'remote-transform-3d+get-remote-node :set 'remote-transform-3d+set-remote-node)

(defgproperty remote-transform-3d+use-global-coordinates 'remote-transform-3d
 :get 'remote-transform-3d+get-use-global-coordinates :set
 'remote-transform-3d+set-use-global-coordinates)

(defgproperty remote-transform-3d+update-position 'remote-transform-3d :get
 'remote-transform-3d+get-update-position :set
 'remote-transform-3d+set-update-position)

(defgproperty remote-transform-3d+update-rotation 'remote-transform-3d :get
 'remote-transform-3d+get-update-rotation :set
 'remote-transform-3d+set-update-rotation)

(defgproperty remote-transform-3d+update-scale 'remote-transform-3d :get
 'remote-transform-3d+get-update-scale :set
 'remote-transform-3d+set-update-scale)