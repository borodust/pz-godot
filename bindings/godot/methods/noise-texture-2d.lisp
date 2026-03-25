(common-lisp:in-package :%godot)


(defgmethod
 (noise-texture-2d+set-width :class 'noise-texture-2d :bind "set_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (noise-texture-2d+set-height :class 'noise-texture-2d :bind "set_height" :hash
  1286410249)
 :void (height int))

(defgmethod
 (noise-texture-2d+set-generate-mipmaps :class 'noise-texture-2d :bind
  "set_generate_mipmaps" :hash 2586408642)
 :void (invert bool))

(defgmethod
 (noise-texture-2d+is-generating-mipmaps :class 'noise-texture-2d :bind
  "is_generating_mipmaps" :hash 36873697)
 bool)

(defgmethod
 (noise-texture-2d+set-noise :class 'noise-texture-2d :bind "set_noise" :hash
  4135492439)
 :void (noise noise))

(defgmethod
 (noise-texture-2d+get-noise :class 'noise-texture-2d :bind "get_noise" :hash
  185851837)
 noise)

(defgmethod
 (noise-texture-2d+set-color-ramp :class 'noise-texture-2d :bind
  "set_color_ramp" :hash 2756054477)
 :void (gradient gradient))

(defgmethod
 (noise-texture-2d+get-color-ramp :class 'noise-texture-2d :bind
  "get_color_ramp" :hash 132272999)
 gradient)

(defgmethod
 (noise-texture-2d+set-seamless :class 'noise-texture-2d :bind "set_seamless"
  :hash 2586408642)
 :void (seamless bool))

(defgmethod
 (noise-texture-2d+get-seamless :class 'noise-texture-2d :bind "get_seamless"
  :hash 2240911060)
 bool)

(defgmethod
 (noise-texture-2d+set-invert :class 'noise-texture-2d :bind "set_invert" :hash
  2586408642)
 :void (invert bool))

(defgmethod
 (noise-texture-2d+get-invert :class 'noise-texture-2d :bind "get_invert" :hash
  36873697)
 bool)

(defgmethod
 (noise-texture-2d+set-in-3d-space :class 'noise-texture-2d :bind
  "set_in_3d_space" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (noise-texture-2d+is-in-3d-space :class 'noise-texture-2d :bind
  "is_in_3d_space" :hash 36873697)
 bool)

(defgmethod
 (noise-texture-2d+set-as-normal-map :class 'noise-texture-2d :bind
  "set_as_normal_map" :hash 2586408642)
 :void (as-normal-map bool))

(defgmethod
 (noise-texture-2d+is-normal-map :class 'noise-texture-2d :bind "is_normal_map"
  :hash 2240911060)
 bool)

(defgmethod
 (noise-texture-2d+set-normalize :class 'noise-texture-2d :bind "set_normalize"
  :hash 2586408642)
 :void (normalize bool))

(defgmethod
 (noise-texture-2d+is-normalized :class 'noise-texture-2d :bind "is_normalized"
  :hash 36873697)
 bool)

(defgmethod
 (noise-texture-2d+set-seamless-blend-skirt :class 'noise-texture-2d :bind
  "set_seamless_blend_skirt" :hash 373806689)
 :void (seamless-blend-skirt float))

(defgmethod
 (noise-texture-2d+get-seamless-blend-skirt :class 'noise-texture-2d :bind
  "get_seamless_blend_skirt" :hash 191475506)
 float)

(defgmethod
 (noise-texture-2d+set-bump-strength :class 'noise-texture-2d :bind
  "set_bump_strength" :hash 373806689)
 :void (bump-strength float))

(defgmethod
 (noise-texture-2d+get-bump-strength :class 'noise-texture-2d :bind
  "get_bump_strength" :hash 191475506)
 float)