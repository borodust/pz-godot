(common-lisp:in-package :%godot)


(defgmethod
 (ray-cast-3d+set-enabled :class 'ray-cast-3d :bind "set_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (ray-cast-3d+is-enabled :class 'ray-cast-3d :bind "is_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-target-position :class 'ray-cast-3d :bind
  "set_target_position" :hash 3460891852)
 :void (local-point vector-3))

(defgmethod
 (ray-cast-3d+get-target-position :class 'ray-cast-3d :bind
  "get_target_position" :hash 3360562783)
 vector-3)

(defgmethod
 (ray-cast-3d+is-colliding :class 'ray-cast-3d :bind "is_colliding" :hash
  36873697)
 bool)

(defgmethod
 (ray-cast-3d+force-raycast-update :class 'ray-cast-3d :bind
  "force_raycast_update" :hash 3218959716)
 :void)

(defgmethod
 (ray-cast-3d+get-collider :class 'ray-cast-3d :bind "get_collider" :hash
  1981248198)
 object)

(defgmethod
 (ray-cast-3d+get-collider-rid :class 'ray-cast-3d :bind "get_collider_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (ray-cast-3d+get-collider-shape :class 'ray-cast-3d :bind "get_collider_shape"
  :hash 3905245786)
 int)

(defgmethod
 (ray-cast-3d+get-collision-point :class 'ray-cast-3d :bind
  "get_collision_point" :hash 3360562783)
 vector-3)

(defgmethod
 (ray-cast-3d+get-collision-normal :class 'ray-cast-3d :bind
  "get_collision_normal" :hash 3360562783)
 vector-3)

(defgmethod
 (ray-cast-3d+get-collision-face-index :class 'ray-cast-3d :bind
  "get_collision_face_index" :hash 3905245786)
 int)

(defgmethod
 (ray-cast-3d+add-exception-rid :class 'ray-cast-3d :bind "add_exception_rid"
  :hash 2722037293)
 :void (rid rid))

(defgmethod
 (ray-cast-3d+add-exception :class 'ray-cast-3d :bind "add_exception" :hash
  1976431078)
 :void (node collision-object-3d))

(defgmethod
 (ray-cast-3d+remove-exception-rid :class 'ray-cast-3d :bind
  "remove_exception_rid" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (ray-cast-3d+remove-exception :class 'ray-cast-3d :bind "remove_exception"
  :hash 1976431078)
 :void (node collision-object-3d))

(defgmethod
 (ray-cast-3d+clear-exceptions :class 'ray-cast-3d :bind "clear_exceptions"
  :hash 3218959716)
 :void)

(defgmethod
 (ray-cast-3d+set-collision-mask :class 'ray-cast-3d :bind "set_collision_mask"
  :hash 1286410249)
 :void (mask int))

(defgmethod
 (ray-cast-3d+get-collision-mask :class 'ray-cast-3d :bind "get_collision_mask"
  :hash 3905245786)
 int)

(defgmethod
 (ray-cast-3d+set-collision-mask-value :class 'ray-cast-3d :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (ray-cast-3d+get-collision-mask-value :class 'ray-cast-3d :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (ray-cast-3d+set-exclude-parent-body :class 'ray-cast-3d :bind
  "set_exclude_parent_body" :hash 2586408642)
 :void (mask bool))

(defgmethod
 (ray-cast-3d+get-exclude-parent-body :class 'ray-cast-3d :bind
  "get_exclude_parent_body" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-collide-with-areas :class 'ray-cast-3d :bind
  "set_collide_with_areas" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-3d+is-collide-with-areas-enabled :class 'ray-cast-3d :bind
  "is_collide_with_areas_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-collide-with-bodies :class 'ray-cast-3d :bind
  "set_collide_with_bodies" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-3d+is-collide-with-bodies-enabled :class 'ray-cast-3d :bind
  "is_collide_with_bodies_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-hit-from-inside :class 'ray-cast-3d :bind
  "set_hit_from_inside" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-3d+is-hit-from-inside-enabled :class 'ray-cast-3d :bind
  "is_hit_from_inside_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-hit-back-faces :class 'ray-cast-3d :bind "set_hit_back_faces"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (ray-cast-3d+is-hit-back-faces-enabled :class 'ray-cast-3d :bind
  "is_hit_back_faces_enabled" :hash 36873697)
 bool)

(defgmethod
 (ray-cast-3d+set-debug-shape-custom-color :class 'ray-cast-3d :bind
  "set_debug_shape_custom_color" :hash 2920490490)
 :void (debug-shape-custom-color color))

(defgmethod
 (ray-cast-3d+get-debug-shape-custom-color :class 'ray-cast-3d :bind
  "get_debug_shape_custom_color" :hash 3444240500)
 color)

(defgmethod
 (ray-cast-3d+set-debug-shape-thickness :class 'ray-cast-3d :bind
  "set_debug_shape_thickness" :hash 1286410249)
 :void (debug-shape-thickness int))

(defgmethod
 (ray-cast-3d+get-debug-shape-thickness :class 'ray-cast-3d :bind
  "get_debug_shape_thickness" :hash 3905245786)
 int)