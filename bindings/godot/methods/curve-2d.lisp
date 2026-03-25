(common-lisp:in-package :%godot)


(defgmethod
 (curve-2d+get-point-count :class 'curve-2d :bind "get_point_count" :hash
  3905245786)
 int)

(defgmethod
 (curve-2d+set-point-count :class 'curve-2d :bind "set_point_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (curve-2d+add-point :class 'curve-2d :bind "add_point" :hash 4175465202) :void
 (position vector-2) (in vector-2) (out vector-2) (index int))

(defgmethod
 (curve-2d+set-point-position :class 'curve-2d :bind "set_point_position" :hash
  163021252)
 :void (idx int) (position vector-2))

(defgmethod
 (curve-2d+get-point-position :class 'curve-2d :bind "get_point_position" :hash
  2299179447)
 vector-2 (idx int))

(defgmethod
 (curve-2d+set-point-in :class 'curve-2d :bind "set_point_in" :hash 163021252)
 :void (idx int) (position vector-2))

(defgmethod
 (curve-2d+get-point-in :class 'curve-2d :bind "get_point_in" :hash 2299179447)
 vector-2 (idx int))

(defgmethod
 (curve-2d+set-point-out :class 'curve-2d :bind "set_point_out" :hash
  163021252)
 :void (idx int) (position vector-2))

(defgmethod
 (curve-2d+get-point-out :class 'curve-2d :bind "get_point_out" :hash
  2299179447)
 vector-2 (idx int))

(defgmethod
 (curve-2d+remove-point :class 'curve-2d :bind "remove_point" :hash 1286410249)
 :void (idx int))

(defgmethod
 (curve-2d+clear-points :class 'curve-2d :bind "clear_points" :hash 3218959716)
 :void)

(defgmethod (curve-2d+sample :class 'curve-2d :bind "sample" :hash 26514310)
 vector-2 (idx int) (t float))

(defgmethod
 (curve-2d+samplef :class 'curve-2d :bind "samplef" :hash 3588506812) vector-2
 (fofs float))

(defgmethod
 (curve-2d+set-bake-interval :class 'curve-2d :bind "set_bake_interval" :hash
  373806689)
 :void (distance float))

(defgmethod
 (curve-2d+get-bake-interval :class 'curve-2d :bind "get_bake_interval" :hash
  1740695150)
 float)

(defgmethod
 (curve-2d+get-baked-length :class 'curve-2d :bind "get_baked_length" :hash
  1740695150)
 float)

(defgmethod
 (curve-2d+sample-baked :class 'curve-2d :bind "sample_baked" :hash 3464257706)
 vector-2 (offset float) (cubic bool))

(defgmethod
 (curve-2d+sample-baked-with-rotation :class 'curve-2d :bind
  "sample_baked_with_rotation" :hash 3296056341)
 transform-2d (offset float) (cubic bool))

(defgmethod
 (curve-2d+get-baked-points :class 'curve-2d :bind "get_baked_points" :hash
  2961356807)
 packed-vector-2array)

(defgmethod
 (curve-2d+get-closest-point :class 'curve-2d :bind "get_closest_point" :hash
  2656412154)
 vector-2 (to-point vector-2))

(defgmethod
 (curve-2d+get-closest-offset :class 'curve-2d :bind "get_closest_offset" :hash
  2276447920)
 float (to-point vector-2))

(defgmethod
 (curve-2d+tessellate :class 'curve-2d :bind "tessellate" :hash 958145977)
 packed-vector-2array (max-stages int) (tolerance-degrees float))

(defgmethod
 (curve-2d+tessellate-even-length :class 'curve-2d :bind
  "tessellate_even_length" :hash 2319761637)
 packed-vector-2array (max-stages int) (tolerance-length float))