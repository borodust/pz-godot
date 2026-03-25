(common-lisp:in-package :%godot)


(defgmethod
 (style-box-flat+set-bg-color :class 'style-box-flat :bind "set_bg_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (style-box-flat+get-bg-color :class 'style-box-flat :bind "get_bg_color" :hash
  3444240500)
 color)

(defgmethod
 (style-box-flat+set-border-color :class 'style-box-flat :bind
  "set_border_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (style-box-flat+get-border-color :class 'style-box-flat :bind
  "get_border_color" :hash 3444240500)
 color)

(defgmethod
 (style-box-flat+set-border-width-all :class 'style-box-flat :bind
  "set_border_width_all" :hash 1286410249)
 :void (width int))

(defgmethod
 (style-box-flat+get-border-width-min :class 'style-box-flat :bind
  "get_border_width_min" :hash 3905245786)
 int)

(defgmethod
 (style-box-flat+set-border-width :class 'style-box-flat :bind
  "set_border_width" :hash 437707142)
 :void (margin side) (width int))

(defgmethod
 (style-box-flat+get-border-width :class 'style-box-flat :bind
  "get_border_width" :hash 1983885014)
 int (margin side))

(defgmethod
 (style-box-flat+set-border-blend :class 'style-box-flat :bind
  "set_border_blend" :hash 2586408642)
 :void (blend bool))

(defgmethod
 (style-box-flat+get-border-blend :class 'style-box-flat :bind
  "get_border_blend" :hash 36873697)
 bool)

(defgmethod
 (style-box-flat+set-corner-radius-all :class 'style-box-flat :bind
  "set_corner_radius_all" :hash 1286410249)
 :void (radius int))

(defgmethod
 (style-box-flat+set-corner-radius :class 'style-box-flat :bind
  "set_corner_radius" :hash 2696158768)
 :void (corner corner) (radius int))

(defgmethod
 (style-box-flat+get-corner-radius :class 'style-box-flat :bind
  "get_corner_radius" :hash 3982397690)
 int (corner corner))

(defgmethod
 (style-box-flat+set-expand-margin :class 'style-box-flat :bind
  "set_expand_margin" :hash 4290182280)
 :void (margin side) (size float))

(defgmethod
 (style-box-flat+set-expand-margin-all :class 'style-box-flat :bind
  "set_expand_margin_all" :hash 373806689)
 :void (size float))

(defgmethod
 (style-box-flat+get-expand-margin :class 'style-box-flat :bind
  "get_expand_margin" :hash 2869120046)
 float (margin side))

(defgmethod
 (style-box-flat+set-draw-center :class 'style-box-flat :bind "set_draw_center"
  :hash 2586408642)
 :void (draw-center bool))

(defgmethod
 (style-box-flat+is-draw-center-enabled :class 'style-box-flat :bind
  "is_draw_center_enabled" :hash 36873697)
 bool)

(defgmethod
 (style-box-flat+set-skew :class 'style-box-flat :bind "set_skew" :hash
  743155724)
 :void (skew vector-2))

(defgmethod
 (style-box-flat+get-skew :class 'style-box-flat :bind "get_skew" :hash
  3341600327)
 vector-2)

(defgmethod
 (style-box-flat+set-shadow-color :class 'style-box-flat :bind
  "set_shadow_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (style-box-flat+get-shadow-color :class 'style-box-flat :bind
  "get_shadow_color" :hash 3444240500)
 color)

(defgmethod
 (style-box-flat+set-shadow-size :class 'style-box-flat :bind "set_shadow_size"
  :hash 1286410249)
 :void (size int))

(defgmethod
 (style-box-flat+get-shadow-size :class 'style-box-flat :bind "get_shadow_size"
  :hash 3905245786)
 int)

(defgmethod
 (style-box-flat+set-shadow-offset :class 'style-box-flat :bind
  "set_shadow_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (style-box-flat+get-shadow-offset :class 'style-box-flat :bind
  "get_shadow_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (style-box-flat+set-anti-aliased :class 'style-box-flat :bind
  "set_anti_aliased" :hash 2586408642)
 :void (anti-aliased bool))

(defgmethod
 (style-box-flat+is-anti-aliased :class 'style-box-flat :bind "is_anti_aliased"
  :hash 36873697)
 bool)

(defgmethod
 (style-box-flat+set-aa-size :class 'style-box-flat :bind "set_aa_size" :hash
  373806689)
 :void (size float))

(defgmethod
 (style-box-flat+get-aa-size :class 'style-box-flat :bind "get_aa_size" :hash
  1740695150)
 float)

(defgmethod
 (style-box-flat+set-corner-detail :class 'style-box-flat :bind
  "set_corner_detail" :hash 1286410249)
 :void (detail int))

(defgmethod
 (style-box-flat+get-corner-detail :class 'style-box-flat :bind
  "get_corner_detail" :hash 3905245786)
 int)