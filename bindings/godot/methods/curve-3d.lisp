(common-lisp:in-package :%godot)


(defgmethod
 (curve-3d+get-point-count :class 'curve-3d :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (curve-3d+set-point-count :class 'curve-3d :bind "set_point_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (curve-3d+add-point :class 'curve-3d :bind "add_point" :hash 2931053748) :void
 (position vector-3) (in vector-3) (out vector-3) (index int))

(defgmethod
 (curve-3d+set-point-position :class 'curve-3d :bind "set_point_position" :hash
  1530502735)
 :void (idx int) (position vector-3))

(defgmethod
 (curve-3d+get-point-position :class 'curve-3d :bind "get_point_position" :hash
  711720468)
 vector-3 (idx int))

(defgmethod
 (curve-3d+set-point-tilt :class 'curve-3d :bind "set_point_tilt" :hash
  1602489585)
 :void (idx int) (tilt float))

(defgmethod
 (curve-3d+get-point-tilt :class 'curve-3d :bind "get_point_tilt" :hash
  2339986948)
 float (idx int))

(defgmethod
 (curve-3d+set-point-in :class 'curve-3d :bind "set_point_in" :hash 1530502735)
 :void (idx int) (position vector-3))

(defgmethod
 (curve-3d+get-point-in :class 'curve-3d :bind "get_point_in" :hash 711720468)
 vector-3 (idx int))

(defgmethod
 (curve-3d+set-point-out :class 'curve-3d :bind "set_point_out" :hash
  1530502735)
 :void (idx int) (position vector-3))

(defgmethod
 (curve-3d+get-point-out :class 'curve-3d :bind "get_point_out" :hash
  711720468)
 vector-3 (idx int))

(defgmethod
 (curve-3d+remove-point :class 'curve-3d :bind "remove_point" :hash 1286410249)
 :void (idx int))

(defgmethod
 (curve-3d+clear-points :class 'curve-3d :bind "clear_points" :hash 3218959716)
 :void)

(defgmethod (curve-3d+sample :class 'curve-3d :bind "sample" :hash 3285246857)
 vector-3 (idx int) (t float))

(defgmethod
 (curve-3d+samplef :class 'curve-3d :bind "samplef" :hash 2553580215) vector-3
 (fofs float))

(defgmethod
 (curve-3d+set-closed :class 'curve-3d :bind "set_closed" :hash 2586408642)
 :void (closed bool))

(defgmethod
 (curve-3d+is-closed :class 'curve-3d :bind "is_closed" :hash 36873697) bool)

(defgmethod
 (curve-3d+set-bake-interval :class 'curve-3d :bind "set_bake_interval" :hash
  373806689)
 :void (distance float))

(defgmethod
 (curve-3d+get-bake-interval :class 'curve-3d :bind "get_bake_interval" :hash
  1740695150)
 float)

(defgmethod
 (curve-3d+set-up-vector-enabled :class 'curve-3d :bind "set_up_vector_enabled"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (curve-3d+is-up-vector-enabled :class 'curve-3d :bind "is_up_vector_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (curve-3d+get-baked-length :class 'curve-3d :bind "get_baked_length" :hash
  1740695150)
 float)

(defgmethod
 (curve-3d+sample-baked :class 'curve-3d :bind "sample_baked" :hash 1350085894)
 vector-3 (offset float) (cubic bool))

(defgmethod
 (curve-3d+sample-baked-with-rotation :class 'curve-3d :bind
  "sample_baked_with_rotation" :hash 1939359131)
 transform-3d (offset float) (cubic bool) (apply-tilt bool))

(defgmethod
 (curve-3d+sample-baked-up-vector :class 'curve-3d :bind
  "sample_baked_up_vector" :hash 1362627031)
 vector-3 (offset float) (apply-tilt bool))

(defgmethod
 (curve-3d+get-baked-points :class 'curve-3d :bind "get_baked_points" :hash
  497664490)
 packed-vector-3array)

(defgmethod
 (curve-3d+get-baked-tilts :class 'curve-3d :bind "get_baked_tilts" :hash
  675695659)
 packed-float-32array)

(defgmethod
 (curve-3d+get-baked-up-vectors :class 'curve-3d :bind "get_baked_up_vectors"
  :hash 497664490)
 packed-vector-3array)

(defgmethod
 (curve-3d+get-closest-point :class 'curve-3d :bind "get_closest_point" :hash
  192990374)
 vector-3 (to-point vector-3))

(defgmethod
 (curve-3d+get-closest-offset :class 'curve-3d :bind "get_closest_offset" :hash
  1109078154)
 float (to-point vector-3))

(defgmethod
 (curve-3d+tessellate :class 'curve-3d :bind "tessellate" :hash 1519759391)
 packed-vector-3array (max-stages int) (tolerance-degrees float))

(defgmethod
 (curve-3d+tessellate-even-length :class 'curve-3d :bind
  "tessellate_even_length" :hash 133237049)
 packed-vector-3array (max-stages int) (tolerance-length float))