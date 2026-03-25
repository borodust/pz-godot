(common-lisp:in-package :%godot)


(defgproperty physics-ray-query-parameters-2d+from
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+get-from :set
 'physics-ray-query-parameters-2d+set-from)

(defgproperty physics-ray-query-parameters-2d+to
 'physics-ray-query-parameters-2d :get 'physics-ray-query-parameters-2d+get-to
 :set 'physics-ray-query-parameters-2d+set-to)

(defgproperty physics-ray-query-parameters-2d+collision-mask
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+get-collision-mask :set
 'physics-ray-query-parameters-2d+set-collision-mask)

(defgproperty physics-ray-query-parameters-2d+exclude
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+get-exclude :set
 'physics-ray-query-parameters-2d+set-exclude)

(defgproperty physics-ray-query-parameters-2d+collide-with-bodies
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+is-collide-with-bodies-enabled :set
 'physics-ray-query-parameters-2d+set-collide-with-bodies)

(defgproperty physics-ray-query-parameters-2d+collide-with-areas
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+is-collide-with-areas-enabled :set
 'physics-ray-query-parameters-2d+set-collide-with-areas)

(defgproperty physics-ray-query-parameters-2d+hit-from-inside
 'physics-ray-query-parameters-2d :get
 'physics-ray-query-parameters-2d+is-hit-from-inside-enabled :set
 'physics-ray-query-parameters-2d+set-hit-from-inside)