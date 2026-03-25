(common-lisp:in-package :%godot)


(defgmethod
 (retarget-modifier-3d+set-profile :class 'retarget-modifier-3d :bind
  "set_profile" :hash 3870374136)
 :void (profile skeleton-profile))

(defgmethod
 (retarget-modifier-3d+get-profile :class 'retarget-modifier-3d :bind
  "get_profile" :hash 4291782652)
 skeleton-profile)

(defgmethod
 (retarget-modifier-3d+set-use-global-pose :class 'retarget-modifier-3d :bind
  "set_use_global_pose" :hash 2586408642)
 :void (use-global-pose bool))

(defgmethod
 (retarget-modifier-3d+is-using-global-pose :class 'retarget-modifier-3d :bind
  "is_using_global_pose" :hash 36873697)
 bool)

(defgmethod
 (retarget-modifier-3d+set-enable-flags :class 'retarget-modifier-3d :bind
  "set_enable_flags" :hash 2687954213)
 :void (enable-flags retarget-modifier-3d+transform-flag))

(defgmethod
 (retarget-modifier-3d+get-enable-flags :class 'retarget-modifier-3d :bind
  "get_enable_flags" :hash 358995420)
 retarget-modifier-3d+transform-flag)

(defgmethod
 (retarget-modifier-3d+set-position-enabled :class 'retarget-modifier-3d :bind
  "set_position_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (retarget-modifier-3d+is-position-enabled :class 'retarget-modifier-3d :bind
  "is_position_enabled" :hash 36873697)
 bool)

(defgmethod
 (retarget-modifier-3d+set-rotation-enabled :class 'retarget-modifier-3d :bind
  "set_rotation_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (retarget-modifier-3d+is-rotation-enabled :class 'retarget-modifier-3d :bind
  "is_rotation_enabled" :hash 36873697)
 bool)

(defgmethod
 (retarget-modifier-3d+set-scale-enabled :class 'retarget-modifier-3d :bind
  "set_scale_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (retarget-modifier-3d+is-scale-enabled :class 'retarget-modifier-3d :bind
  "is_scale_enabled" :hash 36873697)
 bool)