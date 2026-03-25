(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-space-state-3d+intersect-point :class
  'physics-direct-space-state-3d :bind "intersect_point" :hash 975173756)
 array (parameters physics-point-query-parameters-3d) (max-results int))

(defgmethod
 (physics-direct-space-state-3d+intersect-ray :class
  'physics-direct-space-state-3d :bind "intersect_ray" :hash 3957970750)
 dictionary (parameters physics-ray-query-parameters-3d))

(defgmethod
 (physics-direct-space-state-3d+intersect-shape :class
  'physics-direct-space-state-3d :bind "intersect_shape" :hash 3762137681)
 array (parameters physics-shape-query-parameters-3d) (max-results int))

(defgmethod
 (physics-direct-space-state-3d+cast-motion :class
  'physics-direct-space-state-3d :bind "cast_motion" :hash 1778757334)
 packed-float-32array (parameters physics-shape-query-parameters-3d))

(defgmethod
 (physics-direct-space-state-3d+collide-shape :class
  'physics-direct-space-state-3d :bind "collide_shape" :hash 3762137681)
 array (parameters physics-shape-query-parameters-3d) (max-results int))

(defgmethod
 (physics-direct-space-state-3d+get-rest-info :class
  'physics-direct-space-state-3d :bind "get_rest_info" :hash 1376751592)
 dictionary (parameters physics-shape-query-parameters-3d))