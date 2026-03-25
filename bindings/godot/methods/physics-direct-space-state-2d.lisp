(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-space-state-2d+intersect-point :class
  'physics-direct-space-state-2d :bind "intersect_point" :hash 2118456068)
 array (parameters physics-point-query-parameters-2d) (max-results int))

(defgmethod
 (physics-direct-space-state-2d+intersect-ray :class
  'physics-direct-space-state-2d :bind "intersect_ray" :hash 1590275562)
 dictionary (parameters physics-ray-query-parameters-2d))

(defgmethod
 (physics-direct-space-state-2d+intersect-shape :class
  'physics-direct-space-state-2d :bind "intersect_shape" :hash 2488867228)
 array (parameters physics-shape-query-parameters-2d) (max-results int))

(defgmethod
 (physics-direct-space-state-2d+cast-motion :class
  'physics-direct-space-state-2d :bind "cast_motion" :hash 711275086)
 packed-float-32array (parameters physics-shape-query-parameters-2d))

(defgmethod
 (physics-direct-space-state-2d+collide-shape :class
  'physics-direct-space-state-2d :bind "collide_shape" :hash 2488867228)
 array (parameters physics-shape-query-parameters-2d) (max-results int))

(defgmethod
 (physics-direct-space-state-2d+get-rest-info :class
  'physics-direct-space-state-2d :bind "get_rest_info" :hash 2803666496)
 dictionary (parameters physics-shape-query-parameters-2d))