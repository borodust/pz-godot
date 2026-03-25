(common-lisp:in-package :%godot)


(defgproperty physics-ray-query-parameters-3d+from
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+get-from :set
 'physics-ray-query-parameters-3d+set-from)

(defgproperty physics-ray-query-parameters-3d+to
 'physics-ray-query-parameters-3d :get 'physics-ray-query-parameters-3d+get-to
 :set 'physics-ray-query-parameters-3d+set-to)

(defgproperty physics-ray-query-parameters-3d+collision-mask
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+get-collision-mask :set
 'physics-ray-query-parameters-3d+set-collision-mask)

(defgproperty physics-ray-query-parameters-3d+exclude
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+get-exclude :set
 'physics-ray-query-parameters-3d+set-exclude)

(defgproperty physics-ray-query-parameters-3d+collide-with-bodies
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+is-collide-with-bodies-enabled :set
 'physics-ray-query-parameters-3d+set-collide-with-bodies)

(defgproperty physics-ray-query-parameters-3d+collide-with-areas
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+is-collide-with-areas-enabled :set
 'physics-ray-query-parameters-3d+set-collide-with-areas)

(defgproperty physics-ray-query-parameters-3d+hit-from-inside
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+is-hit-from-inside-enabled :set
 'physics-ray-query-parameters-3d+set-hit-from-inside)

(defgproperty physics-ray-query-parameters-3d+hit-back-faces
 'physics-ray-query-parameters-3d :get
 'physics-ray-query-parameters-3d+is-hit-back-faces-enabled :set
 'physics-ray-query-parameters-3d+set-hit-back-faces)