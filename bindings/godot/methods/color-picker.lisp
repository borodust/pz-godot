(common-lisp:in-package :%godot)


(defgmethod
 (color-picker+set-pick-color :class 'color-picker :bind "set_pick_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (color-picker+get-pick-color :class 'color-picker :bind "get_pick_color" :hash
  3444240500)
 color)

(defgmethod
 (color-picker+set-deferred-mode :class 'color-picker :bind "set_deferred_mode"
  :hash 2586408642)
 :void (mode bool))

(defgmethod
 (color-picker+is-deferred-mode :class 'color-picker :bind "is_deferred_mode"
  :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-color-mode :class 'color-picker :bind "set_color_mode" :hash
  1579114136)
 :void (color-mode color-picker+color-mode-type))

(defgmethod
 (color-picker+get-color-mode :class 'color-picker :bind "get_color_mode" :hash
  392907674)
 color-picker+color-mode-type)

(defgmethod
 (color-picker+set-edit-alpha :class 'color-picker :bind "set_edit_alpha" :hash
  2586408642)
 :void (show bool))

(defgmethod
 (color-picker+is-editing-alpha :class 'color-picker :bind "is_editing_alpha"
  :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-edit-intensity :class 'color-picker :bind
  "set_edit_intensity" :hash 2586408642)
 :void (show bool))

(defgmethod
 (color-picker+is-editing-intensity :class 'color-picker :bind
  "is_editing_intensity" :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-can-add-swatches :class 'color-picker :bind
  "set_can_add_swatches" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (color-picker+are-swatches-enabled :class 'color-picker :bind
  "are_swatches_enabled" :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-presets-visible :class 'color-picker :bind
  "set_presets_visible" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (color-picker+are-presets-visible :class 'color-picker :bind
  "are_presets_visible" :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-modes-visible :class 'color-picker :bind "set_modes_visible"
  :hash 2586408642)
 :void (visible bool))

(defgmethod
 (color-picker+are-modes-visible :class 'color-picker :bind "are_modes_visible"
  :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-sampler-visible :class 'color-picker :bind
  "set_sampler_visible" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (color-picker+is-sampler-visible :class 'color-picker :bind
  "is_sampler_visible" :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-sliders-visible :class 'color-picker :bind
  "set_sliders_visible" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (color-picker+are-sliders-visible :class 'color-picker :bind
  "are_sliders_visible" :hash 36873697)
 bool)

(defgmethod
 (color-picker+set-hex-visible :class 'color-picker :bind "set_hex_visible"
  :hash 2586408642)
 :void (visible bool))

(defgmethod
 (color-picker+is-hex-visible :class 'color-picker :bind "is_hex_visible" :hash
  36873697)
 bool)

(defgmethod
 (color-picker+add-preset :class 'color-picker :bind "add_preset" :hash
  2920490490)
 :void (color color))

(defgmethod
 (color-picker+erase-preset :class 'color-picker :bind "erase_preset" :hash
  2920490490)
 :void (color color))

(defgmethod
 (color-picker+get-presets :class 'color-picker :bind "get_presets" :hash
  1392750486)
 packed-color-array)

(defgmethod
 (color-picker+add-recent-preset :class 'color-picker :bind "add_recent_preset"
  :hash 2920490490)
 :void (color color))

(defgmethod
 (color-picker+erase-recent-preset :class 'color-picker :bind
  "erase_recent_preset" :hash 2920490490)
 :void (color color))

(defgmethod
 (color-picker+get-recent-presets :class 'color-picker :bind
  "get_recent_presets" :hash 1392750486)
 packed-color-array)

(defgmethod
 (color-picker+set-picker-shape :class 'color-picker :bind "set_picker_shape"
  :hash 3981373861)
 :void (shape color-picker+picker-shape-type))

(defgmethod
 (color-picker+get-picker-shape :class 'color-picker :bind "get_picker_shape"
  :hash 1143229889)
 color-picker+picker-shape-type)