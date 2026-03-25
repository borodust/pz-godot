(common-lisp:in-package :%godot)


(defgproperty csgshape-3d+operation 'csgshape-3d :get
 'csgshape-3d+get-operation :set 'csgshape-3d+set-operation)

(defgproperty csgshape-3d+snap 'csgshape-3d :get 'csgshape-3d+get-snap :set
 'csgshape-3d+set-snap)

(defgproperty csgshape-3d+calculate-tangents 'csgshape-3d :get
 'csgshape-3d+is-calculating-tangents :set 'csgshape-3d+set-calculate-tangents)

(defgproperty csgshape-3d+use-collision 'csgshape-3d :get
 'csgshape-3d+is-using-collision :set 'csgshape-3d+set-use-collision)

(defgproperty csgshape-3d+collision-layer 'csgshape-3d :get
 'csgshape-3d+get-collision-layer :set 'csgshape-3d+set-collision-layer)

(defgproperty csgshape-3d+collision-mask 'csgshape-3d :get
 'csgshape-3d+get-collision-mask :set 'csgshape-3d+set-collision-mask)

(defgproperty csgshape-3d+collision-priority 'csgshape-3d :get
 'csgshape-3d+get-collision-priority :set 'csgshape-3d+set-collision-priority)