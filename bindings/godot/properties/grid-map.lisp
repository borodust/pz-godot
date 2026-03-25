(common-lisp:in-package :%godot)


(defgproperty grid-map+mesh-library 'grid-map :get 'grid-map+get-mesh-library
 :set 'grid-map+set-mesh-library)

(defgproperty grid-map+physics-material 'grid-map :get
 'grid-map+get-physics-material :set 'grid-map+set-physics-material)

(defgproperty grid-map+cell-size 'grid-map :get 'grid-map+get-cell-size :set
 'grid-map+set-cell-size)

(defgproperty grid-map+cell-octant-size 'grid-map :get
 'grid-map+get-octant-size :set 'grid-map+set-octant-size)

(defgproperty grid-map+cell-center-x 'grid-map :get 'grid-map+get-center-x :set
 'grid-map+set-center-x)

(defgproperty grid-map+cell-center-y 'grid-map :get 'grid-map+get-center-y :set
 'grid-map+set-center-y)

(defgproperty grid-map+cell-center-z 'grid-map :get 'grid-map+get-center-z :set
 'grid-map+set-center-z)

(defgproperty grid-map+cell-scale 'grid-map :get 'grid-map+get-cell-scale :set
 'grid-map+set-cell-scale)

(defgproperty grid-map+collision-layer 'grid-map :get
 'grid-map+get-collision-layer :set 'grid-map+set-collision-layer)

(defgproperty grid-map+collision-mask 'grid-map :get
 'grid-map+get-collision-mask :set 'grid-map+set-collision-mask)

(defgproperty grid-map+collision-priority 'grid-map :get
 'grid-map+get-collision-priority :set 'grid-map+set-collision-priority)

(defgproperty grid-map+bake-navigation 'grid-map :get
 'grid-map+is-baking-navigation :set 'grid-map+set-bake-navigation)