(common-lisp:in-package :%godot)


(defgmethod
 (character-body-3d+move-and-slide :class 'character-body-3d :bind
  "move_and_slide" :hash 2240911060)
 bool)

(defgmethod
 (character-body-3d+apply-floor-snap :class 'character-body-3d :bind
  "apply_floor_snap" :hash 3218959716)
 :void)

(defgmethod
 (character-body-3d+set-velocity :class 'character-body-3d :bind "set_velocity"
  :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (character-body-3d+get-velocity :class 'character-body-3d :bind "get_velocity"
  :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+set-safe-margin :class 'character-body-3d :bind
  "set_safe_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (character-body-3d+get-safe-margin :class 'character-body-3d :bind
  "get_safe_margin" :hash 1740695150)
 float)

(defgmethod
 (character-body-3d+is-floor-stop-on-slope-enabled :class 'character-body-3d
  :bind "is_floor_stop_on_slope_enabled" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+set-floor-stop-on-slope-enabled :class 'character-body-3d
  :bind "set_floor_stop_on_slope_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (character-body-3d+set-floor-constant-speed-enabled :class 'character-body-3d
  :bind "set_floor_constant_speed_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (character-body-3d+is-floor-constant-speed-enabled :class 'character-body-3d
  :bind "is_floor_constant_speed_enabled" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+set-floor-block-on-wall-enabled :class 'character-body-3d
  :bind "set_floor_block_on_wall_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (character-body-3d+is-floor-block-on-wall-enabled :class 'character-body-3d
  :bind "is_floor_block_on_wall_enabled" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+set-slide-on-ceiling-enabled :class 'character-body-3d
  :bind "set_slide_on_ceiling_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (character-body-3d+is-slide-on-ceiling-enabled :class 'character-body-3d :bind
  "is_slide_on_ceiling_enabled" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+set-platform-floor-layers :class 'character-body-3d :bind
  "set_platform_floor_layers" :hash 1286410249)
 :void (exclude-layer int))

(defgmethod
 (character-body-3d+get-platform-floor-layers :class 'character-body-3d :bind
  "get_platform_floor_layers" :hash 3905245786)
 int)

(defgmethod
 (character-body-3d+set-platform-wall-layers :class 'character-body-3d :bind
  "set_platform_wall_layers" :hash 1286410249)
 :void (exclude-layer int))

(defgmethod
 (character-body-3d+get-platform-wall-layers :class 'character-body-3d :bind
  "get_platform_wall_layers" :hash 3905245786)
 int)

(defgmethod
 (character-body-3d+get-max-slides :class 'character-body-3d :bind
  "get_max_slides" :hash 3905245786)
 int)

(defgmethod
 (character-body-3d+set-max-slides :class 'character-body-3d :bind
  "set_max_slides" :hash 1286410249)
 :void (max-slides int))

(defgmethod
 (character-body-3d+get-floor-max-angle :class 'character-body-3d :bind
  "get_floor_max_angle" :hash 1740695150)
 float)

(defgmethod
 (character-body-3d+set-floor-max-angle :class 'character-body-3d :bind
  "set_floor_max_angle" :hash 373806689)
 :void (radians float))

(defgmethod
 (character-body-3d+get-floor-snap-length :class 'character-body-3d :bind
  "get_floor_snap_length" :hash 191475506)
 float)

(defgmethod
 (character-body-3d+set-floor-snap-length :class 'character-body-3d :bind
  "set_floor_snap_length" :hash 373806689)
 :void (floor-snap-length float))

(defgmethod
 (character-body-3d+get-wall-min-slide-angle :class 'character-body-3d :bind
  "get_wall_min_slide_angle" :hash 1740695150)
 float)

(defgmethod
 (character-body-3d+set-wall-min-slide-angle :class 'character-body-3d :bind
  "set_wall_min_slide_angle" :hash 373806689)
 :void (radians float))

(defgmethod
 (character-body-3d+get-up-direction :class 'character-body-3d :bind
  "get_up_direction" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+set-up-direction :class 'character-body-3d :bind
  "set_up_direction" :hash 3460891852)
 :void (up-direction vector-3))

(defgmethod
 (character-body-3d+set-motion-mode :class 'character-body-3d :bind
  "set_motion_mode" :hash 2690739026)
 :void (mode character-body-3d+motion-mode))

(defgmethod
 (character-body-3d+get-motion-mode :class 'character-body-3d :bind
  "get_motion_mode" :hash 3529553604)
 character-body-3d+motion-mode)

(defgmethod
 (character-body-3d+set-platform-on-leave :class 'character-body-3d :bind
  "set_platform_on_leave" :hash 1459986142)
 :void (on-leave-apply-velocity character-body-3d+platform-on-leave))

(defgmethod
 (character-body-3d+get-platform-on-leave :class 'character-body-3d :bind
  "get_platform_on_leave" :hash 996491171)
 character-body-3d+platform-on-leave)

(defgmethod
 (character-body-3d+is-on-floor :class 'character-body-3d :bind "is_on_floor"
  :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+is-on-floor-only :class 'character-body-3d :bind
  "is_on_floor_only" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+is-on-ceiling :class 'character-body-3d :bind
  "is_on_ceiling" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+is-on-ceiling-only :class 'character-body-3d :bind
  "is_on_ceiling_only" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+is-on-wall :class 'character-body-3d :bind "is_on_wall"
  :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+is-on-wall-only :class 'character-body-3d :bind
  "is_on_wall_only" :hash 36873697)
 bool)

(defgmethod
 (character-body-3d+get-floor-normal :class 'character-body-3d :bind
  "get_floor_normal" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-wall-normal :class 'character-body-3d :bind
  "get_wall_normal" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-last-motion :class 'character-body-3d :bind
  "get_last_motion" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-position-delta :class 'character-body-3d :bind
  "get_position_delta" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-real-velocity :class 'character-body-3d :bind
  "get_real_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-floor-angle :class 'character-body-3d :bind
  "get_floor_angle" :hash 2906300789)
 float (up-direction vector-3))

(defgmethod
 (character-body-3d+get-platform-velocity :class 'character-body-3d :bind
  "get_platform_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-platform-angular-velocity :class 'character-body-3d
  :bind "get_platform_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (character-body-3d+get-slide-collision-count :class 'character-body-3d :bind
  "get_slide_collision_count" :hash 3905245786)
 int)

(defgmethod
 (character-body-3d+get-slide-collision :class 'character-body-3d :bind
  "get_slide_collision" :hash 107003663)
 kinematic-collision-3d (slide-idx int))

(defgmethod
 (character-body-3d+get-last-slide-collision :class 'character-body-3d :bind
  "get_last_slide_collision" :hash 186875014)
 kinematic-collision-3d)