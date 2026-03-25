(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-space-state-3dextension+-intersect-ray :class
  'physics-direct-space-state-3dextension :bind "_intersect_ray" :hash
  2022529123 :virtual common-lisp:t)
 bool (from vector-3) (to vector-3) (collision-mask int)
 (collide-with-bodies bool) (collide-with-areas bool) (hit-from-inside bool)
 (hit-back-faces bool) (pick-ray bool)
 (result (:pointer physics-server-3dextension-ray-result)))

(defgmethod
 (physics-direct-space-state-3dextension+-intersect-point :class
  'physics-direct-space-state-3dextension :bind "_intersect_point" :hash
  3378904092 :virtual common-lisp:t)
 int (position vector-3) (collision-mask int) (collide-with-bodies bool)
 (collide-with-areas bool)
 (results (:pointer physics-server-3dextension-shape-result)) (max-results int))

(defgmethod
 (physics-direct-space-state-3dextension+-intersect-shape :class
  'physics-direct-space-state-3dextension :bind "_intersect_shape" :hash
  728953575 :virtual common-lisp:t)
 int (shape-rid rid) (transform transform-3d) (motion vector-3) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (result-count (:pointer physics-server-3dextension-shape-result))
 (max-results int))

(defgmethod
 (physics-direct-space-state-3dextension+-cast-motion :class
  'physics-direct-space-state-3dextension :bind "_cast_motion" :hash 2320624824
  :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-3d) (motion vector-3) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (closest-safe (:pointer :float)) (closest-unsafe (:pointer :float))
 (info (:pointer physics-server-3dextension-shape-rest-info)))

(defgmethod
 (physics-direct-space-state-3dextension+-collide-shape :class
  'physics-direct-space-state-3dextension :bind "_collide_shape" :hash
  2320624824 :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-3d) (motion vector-3) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (results (:pointer :void)) (max-results int) (result-count (:pointer :int32)))

(defgmethod
 (physics-direct-space-state-3dextension+-rest-info :class
  'physics-direct-space-state-3dextension :bind "_rest_info" :hash 856242757
  :virtual common-lisp:t)
 bool (shape-rid rid) (transform transform-3d) (motion vector-3) (margin float)
 (collision-mask int) (collide-with-bodies bool) (collide-with-areas bool)
 (rest-info (:pointer physics-server-3dextension-shape-rest-info)))

(defgmethod
 (physics-direct-space-state-3dextension+-get-closest-point-to-object-volume
  :class 'physics-direct-space-state-3dextension :bind
  "_get_closest_point_to_object_volume" :hash 2056183332 :virtual
  common-lisp:t)
 vector-3 (object rid) (point vector-3))

(defgmethod
 (physics-direct-space-state-3dextension+is-body-excluded-from-query :class
  'physics-direct-space-state-3dextension :bind "is_body_excluded_from_query"
  :hash 4155700596)
 bool (body rid))