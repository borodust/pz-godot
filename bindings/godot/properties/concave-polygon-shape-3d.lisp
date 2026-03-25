(common-lisp:in-package :%godot)


(defgproperty concave-polygon-shape-3d+data 'concave-polygon-shape-3d :get
 'concave-polygon-shape-3d+get-faces :set 'concave-polygon-shape-3d+set-faces)

(defgproperty concave-polygon-shape-3d+backface-collision
 'concave-polygon-shape-3d :get
 'concave-polygon-shape-3d+is-backface-collision-enabled :set
 'concave-polygon-shape-3d+set-backface-collision-enabled)