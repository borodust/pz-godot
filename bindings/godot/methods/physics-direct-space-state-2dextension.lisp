(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-space-state-2dextension+%intersect-ray :class
  'physics-direct-space-state-2dextension :bind "_intersect_ray" :hash
  2840492092 :virtual common-lisp:t)
 bool (from vector-2) (to vector-2) (collision-mask int)
 (collide-with-bodies bool) (collide-with-areas bool) (hit-from-inside bool)
 (r-result (:pointer physics-server-2dextension-ray-result)))

(defgmethod
 (physics-direct-space-state-2dextension+%intersect-point :class
  'physics-direct-space-state-2dextension :bind "_intersect_point" :hash
  522407812 :virtual common-lisp:t)
 int (position vector-2) (canvas-instance-id int) (collision-mask int)
 (collide-with-bodies bool) (collide-with-areas bool)
 (r-results (:pointer physics-server-2dextension-shape-result))
 (max-results int))

(defgmethod
 (physics-direct-space-state-2dextension+%intersect-shape :class
  'physics-direct-space-state-2dextension :bind "_intersect_shape" :hash
  1584897015 :virtual common-lisp:t)
 int (shape-rid rid) (transform transform-2d) (motion vector-2) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (r-result (:pointer physics-server-2dextension-shape-result))
 (max-results int))

(defgmethod
 (physics-direct-space-state-2dextension+%cast-motion :class
  'physics-direct-space-state-2dextension :bind "_cast_motion" :hash 1410701151
  :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-2d) (motion vector-2) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (r-closest-safe (:pointer :float)) (r-closest-unsafe (:pointer :float)))

(defgmethod
 (physics-direct-space-state-2dextension+%collide-shape :class
  'physics-direct-space-state-2dextension :bind "_collide_shape" :hash
  871510130 :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-2d) (motion vector-2) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (r-results (:pointer :void)) (max-results int)
 (r-result-count (:pointer :int32)))

(defgmethod
 (physics-direct-space-state-2dextension+%rest-info :class
  'physics-direct-space-state-2dextension :bind "_rest_info" :hash 772675997
  :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-2d) (motion vector-2) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (r-rest-info (:pointer physics-server-2dextension-shape-rest-info)))

(defgmethod
 (physics-direct-space-state-2dextension+is-body-excluded-from-query :class
  'physics-direct-space-state-2dextension :bind "is_body_excluded_from_query"
  :hash 4155700596)
 bool (body rid))