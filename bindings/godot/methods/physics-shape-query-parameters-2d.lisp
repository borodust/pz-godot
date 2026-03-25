(common-lisp:in-package :%godot)


(defgmethod
 (physics-shape-query-parameters-2d+set-shape :class
  'physics-shape-query-parameters-2d :bind "set_shape" :hash 968641751)
 :void (shape resource))

(defgmethod
 (physics-shape-query-parameters-2d+get-shape :class
  'physics-shape-query-parameters-2d :bind "get_shape" :hash 121922552)
 resource)

(defgmethod
 (physics-shape-query-parameters-2d+set-shape-rid :class
  'physics-shape-query-parameters-2d :bind "set_shape_rid" :hash 2722037293)
 :void (shape rid))

(defgmethod
 (physics-shape-query-parameters-2d+get-shape-rid :class
  'physics-shape-query-parameters-2d :bind "get_shape_rid" :hash 2944877500)
 rid)

(defgmethod
 (physics-shape-query-parameters-2d+set-transform :class
  'physics-shape-query-parameters-2d :bind "set_transform" :hash 2761652528)
 :void (transform transform-2d))

(defgmethod
 (physics-shape-query-parameters-2d+get-transform :class
  'physics-shape-query-parameters-2d :bind "get_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (physics-shape-query-parameters-2d+set-motion :class
  'physics-shape-query-parameters-2d :bind "set_motion" :hash 743155724)
 :void (motion vector-2))

(defgmethod
 (physics-shape-query-parameters-2d+get-motion :class
  'physics-shape-query-parameters-2d :bind "get_motion" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-shape-query-parameters-2d+set-margin :class
  'physics-shape-query-parameters-2d :bind "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (physics-shape-query-parameters-2d+get-margin :class
  'physics-shape-query-parameters-2d :bind "get_margin" :hash 1740695150)
 float)

(defgmethod
 (physics-shape-query-parameters-2d+set-collision-mask :class
  'physics-shape-query-parameters-2d :bind "set_collision_mask" :hash
  1286410249)
 :void (collision-mask int))

(defgmethod
 (physics-shape-query-parameters-2d+get-collision-mask :class
  'physics-shape-query-parameters-2d :bind "get_collision_mask" :hash
  3905245786)
 int)

(defgmethod
 (physics-shape-query-parameters-2d+set-exclude :class
  'physics-shape-query-parameters-2d :bind "set_exclude" :hash 381264803)
 :void (exclude array))

(defgmethod
 (physics-shape-query-parameters-2d+get-exclude :class
  'physics-shape-query-parameters-2d :bind "get_exclude" :hash 3995934104)
 array)

(defgmethod
 (physics-shape-query-parameters-2d+set-collide-with-bodies :class
  'physics-shape-query-parameters-2d :bind "set_collide_with_bodies" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-shape-query-parameters-2d+is-collide-with-bodies-enabled :class
  'physics-shape-query-parameters-2d :bind "is_collide_with_bodies_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-shape-query-parameters-2d+set-collide-with-areas :class
  'physics-shape-query-parameters-2d :bind "set_collide_with_areas" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-shape-query-parameters-2d+is-collide-with-areas-enabled :class
  'physics-shape-query-parameters-2d :bind "is_collide_with_areas_enabled"
  :hash 36873697)
 bool)