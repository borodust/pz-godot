(common-lisp:in-package :%godot)


(defgmethod
 (style-box-texture+set-texture :class 'style-box-texture :bind "set_texture"
  :hash 4051416890)
 :void (texture texture-2d))

(defgmethod
 (style-box-texture+get-texture :class 'style-box-texture :bind "get_texture"
  :hash 3635182373)
 texture-2d)

(defgmethod
 (style-box-texture+set-texture-margin :class 'style-box-texture :bind
  "set_texture_margin" :hash 4290182280)
 :void (margin side) (size float))

(defgmethod
 (style-box-texture+set-texture-margin-all :class 'style-box-texture :bind
  "set_texture_margin_all" :hash 373806689)
 :void (size float))

(defgmethod
 (style-box-texture+get-texture-margin :class 'style-box-texture :bind
  "get_texture_margin" :hash 2869120046)
 float (margin side))

(defgmethod
 (style-box-texture+set-expand-margin :class 'style-box-texture :bind
  "set_expand_margin" :hash 4290182280)
 :void (margin side) (size float))

(defgmethod
 (style-box-texture+set-expand-margin-all :class 'style-box-texture :bind
  "set_expand_margin_all" :hash 373806689)
 :void (size float))

(defgmethod
 (style-box-texture+get-expand-margin :class 'style-box-texture :bind
  "get_expand_margin" :hash 2869120046)
 float (margin side))

(defgmethod
 (style-box-texture+set-region-rect :class 'style-box-texture :bind
  "set_region_rect" :hash 2046264180)
 :void (region rect-2))

(defgmethod
 (style-box-texture+get-region-rect :class 'style-box-texture :bind
  "get_region_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (style-box-texture+set-draw-center :class 'style-box-texture :bind
  "set_draw_center" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (style-box-texture+is-draw-center-enabled :class 'style-box-texture :bind
  "is_draw_center_enabled" :hash 36873697)
 bool)

(defgmethod
 (style-box-texture+set-modulate :class 'style-box-texture :bind "set_modulate"
  :hash 2920490490)
 :void (color color))

(defgmethod
 (style-box-texture+get-modulate :class 'style-box-texture :bind "get_modulate"
  :hash 3444240500)
 color)

(defgmethod
 (style-box-texture+set-h-axis-stretch-mode :class 'style-box-texture :bind
  "set_h_axis_stretch_mode" :hash 2965538783)
 :void (mode style-box-texture+axis-stretch-mode))

(defgmethod
 (style-box-texture+get-h-axis-stretch-mode :class 'style-box-texture :bind
  "get_h_axis_stretch_mode" :hash 3807744063)
 style-box-texture+axis-stretch-mode)

(defgmethod
 (style-box-texture+set-v-axis-stretch-mode :class 'style-box-texture :bind
  "set_v_axis_stretch_mode" :hash 2965538783)
 :void (mode style-box-texture+axis-stretch-mode))

(defgmethod
 (style-box-texture+get-v-axis-stretch-mode :class 'style-box-texture :bind
  "get_v_axis_stretch_mode" :hash 3807744063)
 style-box-texture+axis-stretch-mode)