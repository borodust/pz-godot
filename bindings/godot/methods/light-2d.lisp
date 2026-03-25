(common-lisp:in-package :%godot)


(defgmethod
 (light-2d+set-enabled :class 'light-2d :bind "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (light-2d+is-enabled :class 'light-2d :bind "is_enabled" :hash 36873697) bool)

(defgmethod
 (light-2d+set-editor-only :class 'light-2d :bind "set_editor_only" :hash
  2586408642)
 :void (editor-only bool))

(defgmethod
 (light-2d+is-editor-only :class 'light-2d :bind "is_editor_only" :hash
  36873697)
 bool)

(defgmethod
 (light-2d+set-color :class 'light-2d :bind "set_color" :hash 2920490490) :void
 (color color))

(defgmethod
 (light-2d+get-color :class 'light-2d :bind "get_color" :hash 3444240500) color)

(defgmethod
 (light-2d+set-energy :class 'light-2d :bind "set_energy" :hash 373806689)
 :void (energy float))

(defgmethod
 (light-2d+get-energy :class 'light-2d :bind "get_energy" :hash 1740695150)
 float)

(defgmethod
 (light-2d+set-z-range-min :class 'light-2d :bind "set_z_range_min" :hash
  1286410249)
 :void (z int))

(defgmethod
 (light-2d+get-z-range-min :class 'light-2d :bind "get_z_range_min" :hash
  3905245786)
 int)

(defgmethod
 (light-2d+set-z-range-max :class 'light-2d :bind "set_z_range_max" :hash
  1286410249)
 :void (z int))

(defgmethod
 (light-2d+get-z-range-max :class 'light-2d :bind "get_z_range_max" :hash
  3905245786)
 int)

(defgmethod
 (light-2d+set-layer-range-min :class 'light-2d :bind "set_layer_range_min"
  :hash 1286410249)
 :void (layer int))

(defgmethod
 (light-2d+get-layer-range-min :class 'light-2d :bind "get_layer_range_min"
  :hash 3905245786)
 int)

(defgmethod
 (light-2d+set-layer-range-max :class 'light-2d :bind "set_layer_range_max"
  :hash 1286410249)
 :void (layer int))

(defgmethod
 (light-2d+get-layer-range-max :class 'light-2d :bind "get_layer_range_max"
  :hash 3905245786)
 int)

(defgmethod
 (light-2d+set-item-cull-mask :class 'light-2d :bind "set_item_cull_mask" :hash
  1286410249)
 :void (item-cull-mask int))

(defgmethod
 (light-2d+get-item-cull-mask :class 'light-2d :bind "get_item_cull_mask" :hash
  3905245786)
 int)

(defgmethod
 (light-2d+set-item-shadow-cull-mask :class 'light-2d :bind
  "set_item_shadow_cull_mask" :hash 1286410249)
 :void (item-shadow-cull-mask int))

(defgmethod
 (light-2d+get-item-shadow-cull-mask :class 'light-2d :bind
  "get_item_shadow_cull_mask" :hash 3905245786)
 int)

(defgmethod
 (light-2d+set-shadow-enabled :class 'light-2d :bind "set_shadow_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (light-2d+is-shadow-enabled :class 'light-2d :bind "is_shadow_enabled" :hash
  36873697)
 bool)

(defgmethod
 (light-2d+set-shadow-smooth :class 'light-2d :bind "set_shadow_smooth" :hash
  373806689)
 :void (smooth float))

(defgmethod
 (light-2d+get-shadow-smooth :class 'light-2d :bind "get_shadow_smooth" :hash
  1740695150)
 float)

(defgmethod
 (light-2d+set-shadow-filter :class 'light-2d :bind "set_shadow_filter" :hash
  3209356555)
 :void (filter light-2d+shadow-filter))

(defgmethod
 (light-2d+get-shadow-filter :class 'light-2d :bind "get_shadow_filter" :hash
  1973619177)
 light-2d+shadow-filter)

(defgmethod
 (light-2d+set-shadow-color :class 'light-2d :bind "set_shadow_color" :hash
  2920490490)
 :void (shadow-color color))

(defgmethod
 (light-2d+get-shadow-color :class 'light-2d :bind "get_shadow_color" :hash
  3444240500)
 color)

(defgmethod
 (light-2d+set-blend-mode :class 'light-2d :bind "set_blend_mode" :hash
  2916638796)
 :void (mode light-2d+blend-mode))

(defgmethod
 (light-2d+get-blend-mode :class 'light-2d :bind "get_blend_mode" :hash
  936255250)
 light-2d+blend-mode)

(defgmethod
 (light-2d+set-height :class 'light-2d :bind "set_height" :hash 373806689)
 :void (height float))

(defgmethod
 (light-2d+get-height :class 'light-2d :bind "get_height" :hash 1740695150)
 float)