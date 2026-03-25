(common-lisp:in-package :%godot)


(defgmethod
 (noise-texture-3d+set-width :class 'noise-texture-3d :bind "set_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (noise-texture-3d+set-height :class 'noise-texture-3d :bind "set_height" :hash
  1286410249)
 :void (height int))

(defgmethod
 (noise-texture-3d+set-depth :class 'noise-texture-3d :bind "set_depth" :hash
  1286410249)
 :void (depth int))

(defgmethod
 (noise-texture-3d+set-noise :class 'noise-texture-3d :bind "set_noise" :hash
  4135492439)
 :void (noise noise))

(defgmethod
 (noise-texture-3d+get-noise :class 'noise-texture-3d :bind "get_noise" :hash
  185851837)
 noise)

(defgmethod
 (noise-texture-3d+set-color-ramp :class 'noise-texture-3d :bind
  "set_color_ramp" :hash 2756054477)
 :void (gradient gradient))

(defgmethod
 (noise-texture-3d+get-color-ramp :class 'noise-texture-3d :bind
  "get_color_ramp" :hash 132272999)
 gradient)

(defgmethod
 (noise-texture-3d+set-seamless :class 'noise-texture-3d :bind "set_seamless"
  :hash 2586408642)
 :void (seamless bool))

(defgmethod
 (noise-texture-3d+get-seamless :class 'noise-texture-3d :bind "get_seamless"
  :hash 2240911060)
 bool)

(defgmethod
 (noise-texture-3d+set-invert :class 'noise-texture-3d :bind "set_invert" :hash
  2586408642)
 :void (invert bool))

(defgmethod
 (noise-texture-3d+get-invert :class 'noise-texture-3d :bind "get_invert" :hash
  36873697)
 bool)

(defgmethod
 (noise-texture-3d+set-normalize :class 'noise-texture-3d :bind "set_normalize"
  :hash 2586408642)
 :void (normalize bool))

(defgmethod
 (noise-texture-3d+is-normalized :class 'noise-texture-3d :bind "is_normalized"
  :hash 36873697)
 bool)

(defgmethod
 (noise-texture-3d+set-seamless-blend-skirt :class 'noise-texture-3d :bind
  "set_seamless_blend_skirt" :hash 373806689)
 :void (seamless-blend-skirt float))

(defgmethod
 (noise-texture-3d+get-seamless-blend-skirt :class 'noise-texture-3d :bind
  "get_seamless_blend_skirt" :hash 191475506)
 float)