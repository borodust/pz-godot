(common-lisp:in-package :%godot)


(defgmethod
 (physics-shape-query-parameters-3d+set-shape :class
  'physics-shape-query-parameters-3d :bind "set_shape" :hash 968641751)
 :void (shape resource))

(defgmethod
 (physics-shape-query-parameters-3d+get-shape :class
  'physics-shape-query-parameters-3d :bind "get_shape" :hash 121922552)
 resource)

(defgmethod
 (physics-shape-query-parameters-3d+set-shape-rid :class
  'physics-shape-query-parameters-3d :bind "set_shape_rid" :hash 2722037293)
 :void (shape rid))

(defgmethod
 (physics-shape-query-parameters-3d+get-shape-rid :class
  'physics-shape-query-parameters-3d :bind "get_shape_rid" :hash 2944877500)
 rid)

(defgmethod
 (physics-shape-query-parameters-3d+set-transform :class
  'physics-shape-query-parameters-3d :bind "set_transform" :hash 2952846383)
 :void (transform transform-3d))

(defgmethod
 (physics-shape-query-parameters-3d+get-transform :class
  'physics-shape-query-parameters-3d :bind "get_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (physics-shape-query-parameters-3d+set-motion :class
  'physics-shape-query-parameters-3d :bind "set_motion" :hash 3460891852)
 :void (motion vector-3))

(defgmethod
 (physics-shape-query-parameters-3d+get-motion :class
  'physics-shape-query-parameters-3d :bind "get_motion" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-shape-query-parameters-3d+set-margin :class
  'physics-shape-query-parameters-3d :bind "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (physics-shape-query-parameters-3d+get-margin :class
  'physics-shape-query-parameters-3d :bind "get_margin" :hash 1740695150)
 float)

(defgmethod
 (physics-shape-query-parameters-3d+set-collision-mask :class
  'physics-shape-query-parameters-3d :bind "set_collision_mask" :hash
  1286410249)
 :void (collision-mask int))

(defgmethod
 (physics-shape-query-parameters-3d+get-collision-mask :class
  'physics-shape-query-parameters-3d :bind "get_collision_mask" :hash
  3905245786)
 int)

(defgmethod
 (physics-shape-query-parameters-3d+set-exclude :class
  'physics-shape-query-parameters-3d :bind "set_exclude" :hash 381264803)
 :void (exclude array))

(defgmethod
 (physics-shape-query-parameters-3d+get-exclude :class
  'physics-shape-query-parameters-3d :bind "get_exclude" :hash 3995934104)
 array)

(defgmethod
 (physics-shape-query-parameters-3d+set-collide-with-bodies :class
  'physics-shape-query-parameters-3d :bind "set_collide_with_bodies" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-shape-query-parameters-3d+is-collide-with-bodies-enabled :class
  'physics-shape-query-parameters-3d :bind "is_collide_with_bodies_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-shape-query-parameters-3d+set-collide-with-areas :class
  'physics-shape-query-parameters-3d :bind "set_collide_with_areas" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-shape-query-parameters-3d+is-collide-with-areas-enabled :class
  'physics-shape-query-parameters-3d :bind "is_collide_with_areas_enabled"
  :hash 36873697)
 bool)