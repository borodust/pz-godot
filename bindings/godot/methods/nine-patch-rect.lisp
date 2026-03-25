(common-lisp:in-package :%godot)


(defgmethod
 (nine-patch-rect+set-texture :class 'nine-patch-rect :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (nine-patch-rect+get-texture :class 'nine-patch-rect :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (nine-patch-rect+set-patch-margin :class 'nine-patch-rect :bind
  "set_patch_margin" :hash 437707142)
 :void (margin side) (value int))

(defgmethod
 (nine-patch-rect+get-patch-margin :class 'nine-patch-rect :bind
  "get_patch_margin" :hash 1983885014)
 int (margin side))

(defgmethod
 (nine-patch-rect+set-region-rect :class 'nine-patch-rect :bind
  "set_region_rect" :hash 2046264180)
 :void (rect rect-2))

(defgmethod
 (nine-patch-rect+get-region-rect :class 'nine-patch-rect :bind
  "get_region_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (nine-patch-rect+set-draw-center :class 'nine-patch-rect :bind
  "set_draw_center" :hash 2586408642)
 :void (draw-center bool))

(defgmethod
 (nine-patch-rect+is-draw-center-enabled :class 'nine-patch-rect :bind
  "is_draw_center_enabled" :hash 36873697)
 bool)

(defgmethod
 (nine-patch-rect+set-h-axis-stretch-mode :class 'nine-patch-rect :bind
  "set_h_axis_stretch_mode" :hash 3219608417)
 :void (mode nine-patch-rect+axis-stretch-mode))

(defgmethod
 (nine-patch-rect+get-h-axis-stretch-mode :class 'nine-patch-rect :bind
  "get_h_axis_stretch_mode" :hash 3317113799)
 nine-patch-rect+axis-stretch-mode)

(defgmethod
 (nine-patch-rect+set-v-axis-stretch-mode :class 'nine-patch-rect :bind
  "set_v_axis_stretch_mode" :hash 3219608417)
 :void (mode nine-patch-rect+axis-stretch-mode))

(defgmethod
 (nine-patch-rect+get-v-axis-stretch-mode :class 'nine-patch-rect :bind
  "get_v_axis_stretch_mode" :hash 3317113799)
 nine-patch-rect+axis-stretch-mode)