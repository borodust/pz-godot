(common-lisp:in-package :%godot)


(defgmethod
 (copy-transform-modifier-3d+set-copy-flags :class 'copy-transform-modifier-3d
  :bind "set_copy_flags" :hash 2252507859)
 :void (index int) (copy-flags copy-transform-modifier-3d+transform-flag))

(defgmethod
 (copy-transform-modifier-3d+get-copy-flags :class 'copy-transform-modifier-3d
  :bind "get_copy_flags" :hash 1685185931)
 copy-transform-modifier-3d+transform-flag (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-flags :class 'copy-transform-modifier-3d
  :bind "set_axis_flags" :hash 2044211897)
 :void (index int) (axis-flags copy-transform-modifier-3d+axis-flag))

(defgmethod
 (copy-transform-modifier-3d+get-axis-flags :class 'copy-transform-modifier-3d
  :bind "get_axis_flags" :hash 992162046)
 copy-transform-modifier-3d+axis-flag (index int))

(defgmethod
 (copy-transform-modifier-3d+set-invert-flags :class
  'copy-transform-modifier-3d :bind "set_invert_flags" :hash 2044211897)
 :void (index int) (axis-flags copy-transform-modifier-3d+axis-flag))

(defgmethod
 (copy-transform-modifier-3d+get-invert-flags :class
  'copy-transform-modifier-3d :bind "get_invert_flags" :hash 992162046)
 copy-transform-modifier-3d+axis-flag (index int))

(defgmethod
 (copy-transform-modifier-3d+set-copy-position :class
  'copy-transform-modifier-3d :bind "set_copy_position" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-position-copying :class
  'copy-transform-modifier-3d :bind "is_position_copying" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-copy-rotation :class
  'copy-transform-modifier-3d :bind "set_copy_rotation" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-rotation-copying :class
  'copy-transform-modifier-3d :bind "is_rotation_copying" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-copy-scale :class 'copy-transform-modifier-3d
  :bind "set_copy_scale" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-scale-copying :class
  'copy-transform-modifier-3d :bind "is_scale_copying" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-x-enabled :class
  'copy-transform-modifier-3d :bind "set_axis_x_enabled" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-x-enabled :class
  'copy-transform-modifier-3d :bind "is_axis_x_enabled" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-y-enabled :class
  'copy-transform-modifier-3d :bind "set_axis_y_enabled" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-y-enabled :class
  'copy-transform-modifier-3d :bind "is_axis_y_enabled" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-z-enabled :class
  'copy-transform-modifier-3d :bind "set_axis_z_enabled" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-z-enabled :class
  'copy-transform-modifier-3d :bind "is_axis_z_enabled" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-x-inverted :class
  'copy-transform-modifier-3d :bind "set_axis_x_inverted" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-x-inverted :class
  'copy-transform-modifier-3d :bind "is_axis_x_inverted" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-y-inverted :class
  'copy-transform-modifier-3d :bind "set_axis_y_inverted" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-y-inverted :class
  'copy-transform-modifier-3d :bind "is_axis_y_inverted" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-axis-z-inverted :class
  'copy-transform-modifier-3d :bind "set_axis_z_inverted" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-axis-z-inverted :class
  'copy-transform-modifier-3d :bind "is_axis_z_inverted" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-relative :class 'copy-transform-modifier-3d
  :bind "set_relative" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-relative :class 'copy-transform-modifier-3d
  :bind "is_relative" :hash 1116898809)
 bool (index int))

(defgmethod
 (copy-transform-modifier-3d+set-additive :class 'copy-transform-modifier-3d
  :bind "set_additive" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (copy-transform-modifier-3d+is-additive :class 'copy-transform-modifier-3d
  :bind "is_additive" :hash 1116898809)
 bool (index int))