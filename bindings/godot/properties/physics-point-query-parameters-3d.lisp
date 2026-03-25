(common-lisp:in-package :%godot)


(defgproperty physics-point-query-parameters-3d+position
 'physics-point-query-parameters-3d :get
 'physics-point-query-parameters-3d+get-position :set
 'physics-point-query-parameters-3d+set-position)

(defgproperty physics-point-query-parameters-3d+collision-mask
 'physics-point-query-parameters-3d :get
 'physics-point-query-parameters-3d+get-collision-mask :set
 'physics-point-query-parameters-3d+set-collision-mask)

(defgproperty physics-point-query-parameters-3d+exclude
 'physics-point-query-parameters-3d :get
 'physics-point-query-parameters-3d+get-exclude :set
 'physics-point-query-parameters-3d+set-exclude)

(defgproperty physics-point-query-parameters-3d+collide-with-bodies
 'physics-point-query-parameters-3d :get
 'physics-point-query-parameters-3d+is-collide-with-bodies-enabled :set
 'physics-point-query-parameters-3d+set-collide-with-bodies)

(defgproperty physics-point-query-parameters-3d+collide-with-areas
 'physics-point-query-parameters-3d :get
 'physics-point-query-parameters-3d+is-collide-with-areas-enabled :set
 'physics-point-query-parameters-3d+set-collide-with-areas)