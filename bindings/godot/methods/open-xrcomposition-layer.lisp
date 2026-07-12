(common-lisp:in-package :%godot)


(defgmethod
 (open-xrcomposition-layer+set-layer-viewport :class 'open-xrcomposition-layer
  :bind "set_layer_viewport" :hash 3888077664)
 :void (viewport sub-viewport))

(defgmethod
 (open-xrcomposition-layer+get-layer-viewport :class 'open-xrcomposition-layer
  :bind "get_layer_viewport" :hash 3750751911)
 sub-viewport)

(defgmethod
 (open-xrcomposition-layer+set-use-android-surface :class
  'open-xrcomposition-layer :bind "set_use_android_surface" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (open-xrcomposition-layer+get-use-android-surface :class
  'open-xrcomposition-layer :bind "get_use_android_surface" :hash 36873697)
 bool)

(defgmethod
 (open-xrcomposition-layer+set-android-surface-size :class
  'open-xrcomposition-layer :bind "set_android_surface_size" :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (open-xrcomposition-layer+get-android-surface-size :class
  'open-xrcomposition-layer :bind "get_android_surface_size" :hash 3690982128)
 vector-2i)

(defgmethod
 (open-xrcomposition-layer+set-enable-hole-punch :class
  'open-xrcomposition-layer :bind "set_enable_hole_punch" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (open-xrcomposition-layer+get-enable-hole-punch :class
  'open-xrcomposition-layer :bind "get_enable_hole_punch" :hash 36873697)
 bool)

(defgmethod
 (open-xrcomposition-layer+set-sort-order :class 'open-xrcomposition-layer
  :bind "set_sort_order" :hash 1286410249)
 :void (order int))

(defgmethod
 (open-xrcomposition-layer+get-sort-order :class 'open-xrcomposition-layer
  :bind "get_sort_order" :hash 3905245786)
 int)

(defgmethod
 (open-xrcomposition-layer+set-alpha-blend :class 'open-xrcomposition-layer
  :bind "set_alpha_blend" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (open-xrcomposition-layer+get-alpha-blend :class 'open-xrcomposition-layer
  :bind "get_alpha_blend" :hash 36873697)
 bool)

(defgmethod
 (open-xrcomposition-layer+get-android-surface :class 'open-xrcomposition-layer
  :bind "get_android_surface" :hash 3277089691)
 java-object)

(defgmethod
 (open-xrcomposition-layer+is-natively-supported :class
  'open-xrcomposition-layer :bind "is_natively_supported" :hash 36873697)
 bool)

(defgmethod
 (open-xrcomposition-layer+is-protected-content :class
  'open-xrcomposition-layer :bind "is_protected_content" :hash 36873697)
 bool)

(defgmethod
 (open-xrcomposition-layer+set-protected-content :class
  'open-xrcomposition-layer :bind "set_protected_content" :hash 2586408642)
 :void (protected-content bool))

(defgmethod
 (open-xrcomposition-layer+set-min-filter :class 'open-xrcomposition-layer
  :bind "set_min_filter" :hash 3653437593)
 :void (mode open-xrcomposition-layer+filter))

(defgmethod
 (open-xrcomposition-layer+get-min-filter :class 'open-xrcomposition-layer
  :bind "get_min_filter" :hash 845677307)
 open-xrcomposition-layer+filter)

(defgmethod
 (open-xrcomposition-layer+set-mag-filter :class 'open-xrcomposition-layer
  :bind "set_mag_filter" :hash 3653437593)
 :void (mode open-xrcomposition-layer+filter))

(defgmethod
 (open-xrcomposition-layer+get-mag-filter :class 'open-xrcomposition-layer
  :bind "get_mag_filter" :hash 845677307)
 open-xrcomposition-layer+filter)

(defgmethod
 (open-xrcomposition-layer+set-mipmap-mode :class 'open-xrcomposition-layer
  :bind "set_mipmap_mode" :hash 3271133183)
 :void (mode open-xrcomposition-layer+mipmap-mode))

(defgmethod
 (open-xrcomposition-layer+get-mipmap-mode :class 'open-xrcomposition-layer
  :bind "get_mipmap_mode" :hash 3962697095)
 open-xrcomposition-layer+mipmap-mode)

(defgmethod
 (open-xrcomposition-layer+set-horizontal-wrap :class 'open-xrcomposition-layer
  :bind "set_horizontal_wrap" :hash 15634990)
 :void (mode open-xrcomposition-layer+wrap))

(defgmethod
 (open-xrcomposition-layer+get-horizontal-wrap :class 'open-xrcomposition-layer
  :bind "get_horizontal_wrap" :hash 2798816834)
 open-xrcomposition-layer+wrap)

(defgmethod
 (open-xrcomposition-layer+set-vertical-wrap :class 'open-xrcomposition-layer
  :bind "set_vertical_wrap" :hash 15634990)
 :void (mode open-xrcomposition-layer+wrap))

(defgmethod
 (open-xrcomposition-layer+get-vertical-wrap :class 'open-xrcomposition-layer
  :bind "get_vertical_wrap" :hash 2798816834)
 open-xrcomposition-layer+wrap)

(defgmethod
 (open-xrcomposition-layer+set-red-swizzle :class 'open-xrcomposition-layer
  :bind "set_red_swizzle" :hash 741598951)
 :void (mode open-xrcomposition-layer+swizzle))

(defgmethod
 (open-xrcomposition-layer+get-red-swizzle :class 'open-xrcomposition-layer
  :bind "get_red_swizzle" :hash 2334776767)
 open-xrcomposition-layer+swizzle)

(defgmethod
 (open-xrcomposition-layer+set-green-swizzle :class 'open-xrcomposition-layer
  :bind "set_green_swizzle" :hash 741598951)
 :void (mode open-xrcomposition-layer+swizzle))

(defgmethod
 (open-xrcomposition-layer+get-green-swizzle :class 'open-xrcomposition-layer
  :bind "get_green_swizzle" :hash 2334776767)
 open-xrcomposition-layer+swizzle)

(defgmethod
 (open-xrcomposition-layer+set-blue-swizzle :class 'open-xrcomposition-layer
  :bind "set_blue_swizzle" :hash 741598951)
 :void (mode open-xrcomposition-layer+swizzle))

(defgmethod
 (open-xrcomposition-layer+get-blue-swizzle :class 'open-xrcomposition-layer
  :bind "get_blue_swizzle" :hash 2334776767)
 open-xrcomposition-layer+swizzle)

(defgmethod
 (open-xrcomposition-layer+set-alpha-swizzle :class 'open-xrcomposition-layer
  :bind "set_alpha_swizzle" :hash 741598951)
 :void (mode open-xrcomposition-layer+swizzle))

(defgmethod
 (open-xrcomposition-layer+get-alpha-swizzle :class 'open-xrcomposition-layer
  :bind "get_alpha_swizzle" :hash 2334776767)
 open-xrcomposition-layer+swizzle)

(defgmethod
 (open-xrcomposition-layer+set-max-anisotropy :class 'open-xrcomposition-layer
  :bind "set_max_anisotropy" :hash 373806689)
 :void (value float))

(defgmethod
 (open-xrcomposition-layer+get-max-anisotropy :class 'open-xrcomposition-layer
  :bind "get_max_anisotropy" :hash 1740695150)
 float)

(defgmethod
 (open-xrcomposition-layer+set-border-color :class 'open-xrcomposition-layer
  :bind "set_border_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (open-xrcomposition-layer+get-border-color :class 'open-xrcomposition-layer
  :bind "get_border_color" :hash 3444240500)
 color)

(defgmethod
 (open-xrcomposition-layer+set-eye-visibility :class 'open-xrcomposition-layer
  :bind "set_eye_visibility" :hash 156391336)
 :void (eye-visibility open-xrcomposition-layer+eye-visibility))

(defgmethod
 (open-xrcomposition-layer+get-eye-visibility :class 'open-xrcomposition-layer
  :bind "get_eye_visibility" :hash 467669000)
 open-xrcomposition-layer+eye-visibility)

(defgmethod
 (open-xrcomposition-layer+intersects-ray :class 'open-xrcomposition-layer
  :bind "intersects_ray" :hash 1091262597)
 vector-2 (origin vector-3) (direction vector-3))