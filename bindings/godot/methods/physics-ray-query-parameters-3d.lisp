(common-lisp:in-package :%godot)


(defgmethod
 (physics-ray-query-parameters-3d+create :class
  'physics-ray-query-parameters-3d :bind "create" :hash 3110599579 :static
  common-lisp:t)
 physics-ray-query-parameters-3d (from vector-3) (to vector-3)
 (collision-mask int) (exclude array))

(defgmethod
 (physics-ray-query-parameters-3d+set-from :class
  'physics-ray-query-parameters-3d :bind "set_from" :hash 3460891852)
 :void (from vector-3))

(defgmethod
 (physics-ray-query-parameters-3d+get-from :class
  'physics-ray-query-parameters-3d :bind "get_from" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-ray-query-parameters-3d+set-to :class
  'physics-ray-query-parameters-3d :bind "set_to" :hash 3460891852)
 :void (to vector-3))

(defgmethod
 (physics-ray-query-parameters-3d+get-to :class
  'physics-ray-query-parameters-3d :bind "get_to" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-ray-query-parameters-3d+set-collision-mask :class
  'physics-ray-query-parameters-3d :bind "set_collision_mask" :hash 1286410249)
 :void (collision-mask int))

(defgmethod
 (physics-ray-query-parameters-3d+get-collision-mask :class
  'physics-ray-query-parameters-3d :bind "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (physics-ray-query-parameters-3d+set-exclude :class
  'physics-ray-query-parameters-3d :bind "set_exclude" :hash 381264803)
 :void (exclude array))

(defgmethod
 (physics-ray-query-parameters-3d+get-exclude :class
  'physics-ray-query-parameters-3d :bind "get_exclude" :hash 3995934104)
 array)

(defgmethod
 (physics-ray-query-parameters-3d+set-collide-with-bodies :class
  'physics-ray-query-parameters-3d :bind "set_collide_with_bodies" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-3d+is-collide-with-bodies-enabled :class
  'physics-ray-query-parameters-3d :bind "is_collide_with_bodies_enabled" :hash
  36873697)
 bool)

(defgmethod
 (physics-ray-query-parameters-3d+set-collide-with-areas :class
  'physics-ray-query-parameters-3d :bind "set_collide_with_areas" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-3d+is-collide-with-areas-enabled :class
  'physics-ray-query-parameters-3d :bind "is_collide_with_areas_enabled" :hash
  36873697)
 bool)

(defgmethod
 (physics-ray-query-parameters-3d+set-hit-from-inside :class
  'physics-ray-query-parameters-3d :bind "set_hit_from_inside" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-3d+is-hit-from-inside-enabled :class
  'physics-ray-query-parameters-3d :bind "is_hit_from_inside_enabled" :hash
  36873697)
 bool)

(defgmethod
 (physics-ray-query-parameters-3d+set-hit-back-faces :class
  'physics-ray-query-parameters-3d :bind "set_hit_back_faces" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-3d+is-hit-back-faces-enabled :class
  'physics-ray-query-parameters-3d :bind "is_hit_back_faces_enabled" :hash
  36873697)
 bool)