(common-lisp:in-package :%godot)


(defgmethod
 (physics-ray-query-parameters-2d+create :class
  'physics-ray-query-parameters-2d :bind "create" :hash 3196569324 :static
  common-lisp:t)
 physics-ray-query-parameters-2d (from vector-2) (to vector-2)
 (collision-mask int) (exclude array))

(defgmethod
 (physics-ray-query-parameters-2d+set-from :class
  'physics-ray-query-parameters-2d :bind "set_from" :hash 743155724)
 :void (from vector-2))

(defgmethod
 (physics-ray-query-parameters-2d+get-from :class
  'physics-ray-query-parameters-2d :bind "get_from" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-ray-query-parameters-2d+set-to :class
  'physics-ray-query-parameters-2d :bind "set_to" :hash 743155724)
 :void (to vector-2))

(defgmethod
 (physics-ray-query-parameters-2d+get-to :class
  'physics-ray-query-parameters-2d :bind "get_to" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-ray-query-parameters-2d+set-collision-mask :class
  'physics-ray-query-parameters-2d :bind "set_collision_mask" :hash 1286410249)
 :void (collision-mask int))

(defgmethod
 (physics-ray-query-parameters-2d+get-collision-mask :class
  'physics-ray-query-parameters-2d :bind "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (physics-ray-query-parameters-2d+set-exclude :class
  'physics-ray-query-parameters-2d :bind "set_exclude" :hash 381264803)
 :void (exclude array))

(defgmethod
 (physics-ray-query-parameters-2d+get-exclude :class
  'physics-ray-query-parameters-2d :bind "get_exclude" :hash 3995934104)
 array)

(defgmethod
 (physics-ray-query-parameters-2d+set-collide-with-bodies :class
  'physics-ray-query-parameters-2d :bind "set_collide_with_bodies" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-2d+is-collide-with-bodies-enabled :class
  'physics-ray-query-parameters-2d :bind "is_collide_with_bodies_enabled" :hash
  36873697)
 bool)

(defgmethod
 (physics-ray-query-parameters-2d+set-collide-with-areas :class
  'physics-ray-query-parameters-2d :bind "set_collide_with_areas" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-2d+is-collide-with-areas-enabled :class
  'physics-ray-query-parameters-2d :bind "is_collide_with_areas_enabled" :hash
  36873697)
 bool)

(defgmethod
 (physics-ray-query-parameters-2d+set-hit-from-inside :class
  'physics-ray-query-parameters-2d :bind "set_hit_from_inside" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (physics-ray-query-parameters-2d+is-hit-from-inside-enabled :class
  'physics-ray-query-parameters-2d :bind "is_hit_from_inside_enabled" :hash
  36873697)
 bool)