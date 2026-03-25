(common-lisp:in-package :%godot)


(defgmethod
 (ray-cast-2d+set-enabled :class 'ray-cast-2d :bind "set_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (ray-cast-2d+is-enabled :class 'ray-cast-2d :bind "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-2d+set-target-position :class 'ray-cast-2d :bind
  "set_target_position" :hash 743155724)
 :void (local-point vector-2))

(defgmethod
 (ray-cast-2d+get-target-position :class 'ray-cast-2d :bind
  "get_target_position" :hash 3341600327)
 vector-2)

(defgmethod
 (ray-cast-2d+is-colliding :class 'ray-cast-2d :bind "is_colliding" :hash
  36873697)
 bool)

(defgmethod
 (ray-cast-2d+force-raycast-update :class 'ray-cast-2d :bind
  "force_raycast_update" :hash 3218959716)
 :void)

(defgmethod
 (ray-cast-2d+get-collider :class 'ray-cast-2d :bind "get_collider" :hash
  1981248198)
 object)

(defgmethod
 (ray-cast-2d+get-collider-rid :class 'ray-cast-2d :bind "get_collider_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (ray-cast-2d+get-collider-shape :class 'ray-cast-2d :bind "get_collider_shape"
  :hash 3905245786)
 int)

(defgmethod
 (ray-cast-2d+get-collision-point :class 'ray-cast-2d :bind
  "get_collision_point" :hash 3341600327)
 vector-2)

(defgmethod
 (ray-cast-2d+get-collision-normal :class 'ray-cast-2d :bind
  "get_collision_normal" :hash 3341600327)
 vector-2)

(defgmethod
 (ray-cast-2d+add-exception-rid :class 'ray-cast-2d :bind "add_exception_rid"
  :hash 2722037293)
 :void (rid rid))

(defgmethod
 (ray-cast-2d+add-exception :class 'ray-cast-2d :bind "add_exception" :hash
  3090941106)
 :void (node collision-object-2d))

(defgmethod
 (ray-cast-2d+remove-exception-rid :class 'ray-cast-2d :bind
  "remove_exception_rid" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (ray-cast-2d+remove-exception :class 'ray-cast-2d :bind "remove_exception"
  :hash 3090941106)
 :void (node collision-object-2d))

(defgmethod
 (ray-cast-2d+clear-exceptions :class 'ray-cast-2d :bind "clear_exceptions"
  :hash 3218959716)
 :void)

(defgmethod
 (ray-cast-2d+set-collision-mask :class 'ray-cast-2d :bind "set_collision_mask"
  :hash 1286410249)
 :void (mask int))

(defgmethod
 (ray-cast-2d+get-collision-mask :class 'ray-cast-2d :bind "get_collision_mask"
  :hash 3905245786)
 int)

(defgmethod
 (ray-cast-2d+set-collision-mask-value :class 'ray-cast-2d :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (ray-cast-2d+get-collision-mask-value :class 'ray-cast-2d :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (ray-cast-2d+set-exclude-parent-body :class 'ray-cast-2d :bind
  "set_exclude_parent_body" :hash 2586408642)
 :void (mask bool))

(defgmethod
 (ray-cast-2d+get-exclude-parent-body :class 'ray-cast-2d :bind
  "get_exclude_parent_body" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-2d+set-collide-with-areas :class 'ray-cast-2d :bind
  "set_collide_with_areas" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-2d+is-collide-with-areas-enabled :class 'ray-cast-2d :bind
  "is_collide_with_areas_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-2d+set-collide-with-bodies :class 'ray-cast-2d :bind
  "set_collide_with_bodies" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-2d+is-collide-with-bodies-enabled :class 'ray-cast-2d :bind
  "is_collide_with_bodies_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-2d+set-hit-from-inside :class 'ray-cast-2d :bind
  "set_hit_from_inside" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-2d+is-hit-from-inside-enabled :class 'ray-cast-2d :bind
  "is_hit_from_inside_enabled" :hash 36873697)
 bool)