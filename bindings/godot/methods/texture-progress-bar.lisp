(common-lisp:in-package :%godot)


(defgmethod
 (texture-progress-bar+set-under-texture :class 'texture-progress-bar :bind
  "set_under_texture" :hash 4051416890)
 :void (tex texture-2d))

(defgmethod
 (texture-progress-bar+get-under-texture :class 'texture-progress-bar :bind
  "get_under_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-progress-bar+set-progress-texture :class 'texture-progress-bar :bind
  "set_progress_texture" :hash 4051416890)
 :void (tex texture-2d))

(defgmethod
 (texture-progress-bar+get-progress-texture :class 'texture-progress-bar :bind
  "get_progress_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-progress-bar+set-over-texture :class 'texture-progress-bar :bind
  "set_over_texture" :hash 4051416890)
 :void (tex texture-2d))

(defgmethod
 (texture-progress-bar+get-over-texture :class 'texture-progress-bar :bind
  "get_over_texture" :hash 3635182373)
 texture-2d)

(defgmethod
 (texture-progress-bar+set-fill-mode :class 'texture-progress-bar :bind
  "set_fill_mode" :hash 1286410249)
 :void (mode int))

(defgmethod
 (texture-progress-bar+get-fill-mode :class 'texture-progress-bar :bind
  "get_fill_mode" :hash 2455072627)
 int)

(defgmethod
 (texture-progress-bar+set-tint-under :class 'texture-progress-bar :bind
  "set_tint_under" :hash 2920490490)
 :void (tint color))

(defgmethod
 (texture-progress-bar+get-tint-under :class 'texture-progress-bar :bind
  "get_tint_under" :hash 3444240500)
 color)

(defgmethod
 (texture-progress-bar+set-tint-progress :class 'texture-progress-bar :bind
  "set_tint_progress" :hash 2920490490)
 :void (tint color))

(defgmethod
 (texture-progress-bar+get-tint-progress :class 'texture-progress-bar :bind
  "get_tint_progress" :hash 3444240500)
 color)

(defgmethod
 (texture-progress-bar+set-tint-over :class 'texture-progress-bar :bind
  "set_tint_over" :hash 2920490490)
 :void (tint color))

(defgmethod
 (texture-progress-bar+get-tint-over :class 'texture-progress-bar :bind
  "get_tint_over" :hash 3444240500)
 color)

(defgmethod
 (texture-progress-bar+set-texture-progress-offset :class 'texture-progress-bar
  :bind "set_texture_progress_offset" :hash 743155724)
 :void (offset vector-2))

(defgmethod
 (texture-progress-bar+get-texture-progress-offset :class 'texture-progress-bar
  :bind "get_texture_progress_offset" :hash 3341600327)
 vector-2)

(defgmethod
 (texture-progress-bar+set-radial-initial-angle :class 'texture-progress-bar
  :bind "set_radial_initial_angle" :hash 373806689)
 :void (mode float))

(defgmethod
 (texture-progress-bar+get-radial-initial-angle :class 'texture-progress-bar
  :bind "get_radial_initial_angle" :hash 191475506)
 float)

(defgmethod
 (texture-progress-bar+set-radial-center-offset :class 'texture-progress-bar
  :bind "set_radial_center_offset" :hash 743155724)
 :void (mode vector-2))

(defgmethod
 (texture-progress-bar+get-radial-center-offset :class 'texture-progress-bar
  :bind "get_radial_center_offset" :hash 1497962370)
 vector-2)

(defgmethod
 (texture-progress-bar+set-fill-degrees :class 'texture-progress-bar :bind
  "set_fill_degrees" :hash 373806689)
 :void (mode float))

(defgmethod
 (texture-progress-bar+get-fill-degrees :class 'texture-progress-bar :bind
  "get_fill_degrees" :hash 191475506)
 float)

(defgmethod
 (texture-progress-bar+set-stretch-margin :class 'texture-progress-bar :bind
  "set_stretch_margin" :hash 437707142)
 :void (margin side) (value int))

(defgmethod
 (texture-progress-bar+get-stretch-margin :class 'texture-progress-bar :bind
  "get_stretch_margin" :hash 1983885014)
 int (margin side))

(defgmethod
 (texture-progress-bar+set-nine-patch-stretch :class 'texture-progress-bar
  :bind "set_nine_patch_stretch" :hash 2586408642)
 :void (stretch bool))

(defgmethod
 (texture-progress-bar+get-nine-patch-stretch :class 'texture-progress-bar
  :bind "get_nine_patch_stretch" :hash 36873697)
 bool)