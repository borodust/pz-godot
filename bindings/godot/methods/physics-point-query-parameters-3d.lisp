(common-lisp:in-package :%godot)


(defgmethod
 (physics-point-query-parameters-3d+set-position :class
  'physics-point-query-parameters-3d :bind "set_position" :hash 3460891852)
 :void (position vector-3))

(defgmethod
 (physics-point-query-parameters-3d+get-position :class
  'physics-point-query-parameters-3d :bind "get_position" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-point-query-parameters-3d+set-collision-mask :class
  'physics-point-query-parameters-3d :bind "set_collision_mask" :hash
  1286410249)
 :void (collision-mask int))

(defgmethod
 (physics-point-query-parameters-3d+get-collision-mask :class
  'physics-point-query-parameters-3d :bind "get_collision_mask" :hash
  3905245786)
 int)

(defgmethod
 (physics-point-query-parameters-3d+set-exclude :class
  'physics-point-query-parameters-3d :bind "set_exclude" :hash 381264803)
 :void (exclude array))

(defgmethod
 (physics-point-query-parameters-3d+get-exclude :class
  'physics-point-query-parameters-3d :bind "get_exclude" :hash 3995934104)
 array)

(defgmethod
 (physics-point-query-parameters-3d+set-collide-with-bodies :class
  'physics-point-query-parameters-3d :bind "set_collide_with_bodies" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-point-query-parameters-3d+is-collide-with-bodies-enabled :class
  'physics-point-query-parameters-3d :bind "is_collide_with_bodies_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (physics-point-query-parameters-3d+set-collide-with-areas :class
  'physics-point-query-parameters-3d :bind "set_collide_with_areas" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-point-query-parameters-3d+is-collide-with-areas-enabled :class
  'physics-point-query-parameters-3d :bind "is_collide_with_areas_enabled"
  :hash 36873697)
 bool)