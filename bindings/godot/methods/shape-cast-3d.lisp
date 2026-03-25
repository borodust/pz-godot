(common-lisp:in-package :%godot)


(defgmethod
 (shape-cast-3d+resource-changed :class 'shape-cast-3d :bind "resource_changed"
  :hash 968641751)
 :void (resource resource))

(defgmethod
 (shape-cast-3d+set-enabled :class 'shape-cast-3d :bind "set_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (shape-cast-3d+is-enabled :class 'shape-cast-3d :bind "is_enabled" :hash
  36873697)
 bool)

(defgmethod
 (shape-cast-3d+set-shape :class 'shape-cast-3d :bind "set_shape" :hash
  1549710052)
 :void (shape shape-3d))

(defgmethod
 (shape-cast-3d+get-shape :class 'shape-cast-3d :bind "get_shape" :hash
  3214262478)
 shape-3d)

(defgmethod
 (shape-cast-3d+set-target-position :class 'shape-cast-3d :bind
  "set_target_position" :hash 3460891852)
 :void (local-point vector-3))

(defgmethod
 (shape-cast-3d+get-target-position :class 'shape-cast-3d :bind
  "get_target_position" :hash 3360562783)
 vector-3)

(defgmethod
 (shape-cast-3d+set-margin :class 'shape-cast-3d :bind "set_margin" :hash
  373806689)
 :void (margin float))

(defgmethod
 (shape-cast-3d+get-margin :class 'shape-cast-3d :bind "get_margin" :hash
  1740695150)
 float)

(defgmethod
 (shape-cast-3d+set-max-results :class 'shape-cast-3d :bind "set_max_results"
  :hash 1286410249)
 :void (max-results int))

(defgmethod
 (shape-cast-3d+get-max-results :class 'shape-cast-3d :bind "get_max_results"
  :hash 3905245786)
 int)

(defgmethod
 (shape-cast-3d+is-colliding :class 'shape-cast-3d :bind "is_colliding" :hash
  36873697)
 bool)

(defgmethod
 (shape-cast-3d+get-collision-count :class 'shape-cast-3d :bind
  "get_collision_count" :hash 3905245786)
 int)

(defgmethod
 (shape-cast-3d+force-shapecast-update :class 'shape-cast-3d :bind
  "force_shapecast_update" :hash 3218959716)
 :void)

(defgmethod
 (shape-cast-3d+get-collider :class 'shape-cast-3d :bind "get_collider" :hash
  3332903315)
 object (index int))

(defgmethod
 (shape-cast-3d+get-collider-rid :class 'shape-cast-3d :bind "get_collider_rid"
  :hash 495598643)
 rid (index int))

(defgmethod
 (shape-cast-3d+get-collider-shape :class 'shape-cast-3d :bind
  "get_collider_shape" :hash 923996154)
 int (index int))

(defgmethod
 (shape-cast-3d+get-collision-point :class 'shape-cast-3d :bind
  "get_collision_point" :hash 711720468)
 vector-3 (index int))

(defgmethod
 (shape-cast-3d+get-collision-normal :class 'shape-cast-3d :bind
  "get_collision_normal" :hash 711720468)
 vector-3 (index int))

(defgmethod
 (shape-cast-3d+get-closest-collision-safe-fraction :class 'shape-cast-3d :bind
  "get_closest_collision_safe_fraction" :hash 1740695150)
 float)

(defgmethod
 (shape-cast-3d+get-closest-collision-unsafe-fraction :class 'shape-cast-3d
  :bind "get_closest_collision_unsafe_fraction" :hash 1740695150)
 float)

(defgmethod
 (shape-cast-3d+add-exception-rid :class 'shape-cast-3d :bind
  "add_exception_rid" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (shape-cast-3d+add-exception :class 'shape-cast-3d :bind "add_exception" :hash
  1976431078)
 :void (node collision-object-3d))

(defgmethod
 (shape-cast-3d+remove-exception-rid :class 'shape-cast-3d :bind
  "remove_exception_rid" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (shape-cast-3d+remove-exception :class 'shape-cast-3d :bind "remove_exception"
  :hash 1976431078)
 :void (node collision-object-3d))

(defgmethod
 (shape-cast-3d+clear-exceptions :class 'shape-cast-3d :bind "clear_exceptions"
  :hash 3218959716)
 :void)

(defgmethod
 (shape-cast-3d+set-collision-mask :class 'shape-cast-3d :bind
  "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (shape-cast-3d+get-collision-mask :class 'shape-cast-3d :bind
  "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (shape-cast-3d+set-collision-mask-value :class 'shape-cast-3d :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (shape-cast-3d+get-collision-mask-value :class 'shape-cast-3d :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (shape-cast-3d+set-exclude-parent-body :class 'shape-cast-3d :bind
  "set_exclude_parent_body" :hash 2586408642)
 :void (mask bool))

(defgmethod
 (shape-cast-3d+get-exclude-parent-body :class 'shape-cast-3d :bind
  "get_exclude_parent_body" :hash 36873697)
 bool)

(defgmethod
 (shape-cast-3d+set-collide-with-areas :class 'shape-cast-3d :bind
  "set_collide_with_areas" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (shape-cast-3d+is-collide-with-areas-enabled :class 'shape-cast-3d :bind
  "is_collide_with_areas_enabled" :hash 36873697)
 bool)

(defgmethod
 (shape-cast-3d+set-collide-with-bodies :class 'shape-cast-3d :bind
  "set_collide_with_bodies" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (shape-cast-3d+is-collide-with-bodies-enabled :class 'shape-cast-3d :bind
  "is_collide_with_bodies_enabled" :hash 36873697)
 bool)

(defgmethod
 (shape-cast-3d+get-collision-result :class 'shape-cast-3d :bind
  "get_collision_result" :hash 3995934104)
 array)

(defgmethod
 (shape-cast-3d+set-debug-shape-custom-color :class 'shape-cast-3d :bind
  "set_debug_shape_custom_color" :hash 2920490490)
 :void (debug-shape-custom-color color))

(defgmethod
 (shape-cast-3d+get-debug-shape-custom-color :class 'shape-cast-3d :bind
  "get_debug_shape_custom_color" :hash 3444240500)
 color)