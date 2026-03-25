(common-lisp:in-package :%godot)


(defgmethod
 (light-3d+set-editor-only :class 'light-3d :bind "set_editor_only" :hash
  2586408642)
 :void (editor-only bool))

(defgmethod
 (light-3d+is-editor-only :class 'light-3d :bind "is_editor_only" :hash
  36873697)
 bool)

(defgmethod
 (light-3d+set-param :class 'light-3d :bind "set_param" :hash 1722734213) :void
 (param light-3d+param) (value float))

(defgmethod
 (light-3d+get-param :class 'light-3d :bind "get_param" :hash 1844084987) float
 (param light-3d+param))

(defgmethod
 (light-3d+set-shadow :class 'light-3d :bind "set_shadow" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (light-3d+has-shadow :class 'light-3d :bind "has_shadow" :hash 36873697) bool)

(defgmethod
 (light-3d+set-negative :class 'light-3d :bind "set_negative" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (light-3d+is-negative :class 'light-3d :bind "is_negative" :hash 36873697)
 bool)

(defgmethod
 (light-3d+set-cull-mask :class 'light-3d :bind "set_cull_mask" :hash
  1286410249)
 :void (cull-mask int))

(defgmethod
 (light-3d+get-cull-mask :class 'light-3d :bind "get_cull_mask" :hash
  3905245786)
 int)

(defgmethod
 (light-3d+set-enable-distance-fade :class 'light-3d :bind
  "set_enable_distance_fade" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (light-3d+is-distance-fade-enabled :class 'light-3d :bind
  "is_distance_fade_enabled" :hash 36873697)
 bool)

(defgmethod
 (light-3d+set-distance-fade-begin :class 'light-3d :bind
  "set_distance_fade_begin" :hash 373806689)
 :void (distance float))

(defgmethod
 (light-3d+get-distance-fade-begin :class 'light-3d :bind
  "get_distance_fade_begin" :hash 1740695150)
 float)

(defgmethod
 (light-3d+set-distance-fade-shadow :class 'light-3d :bind
  "set_distance_fade_shadow" :hash 373806689)
 :void (distance float))

(defgmethod
 (light-3d+get-distance-fade-shadow :class 'light-3d :bind
  "get_distance_fade_shadow" :hash 1740695150)
 float)

(defgmethod
 (light-3d+set-distance-fade-length :class 'light-3d :bind
  "set_distance_fade_length" :hash 373806689)
 :void (distance float))

(defgmethod
 (light-3d+get-distance-fade-length :class 'light-3d :bind
  "get_distance_fade_length" :hash 1740695150)
 float)

(defgmethod
 (light-3d+set-color :class 'light-3d :bind "set_color" :hash 2920490490) :void
 (color color))

(defgmethod
 (light-3d+get-color :class 'light-3d :bind "get_color" :hash 3444240500) color)

(defgmethod
 (light-3d+set-shadow-reverse-cull-face :class 'light-3d :bind
  "set_shadow_reverse_cull_face" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (light-3d+get-shadow-reverse-cull-face :class 'light-3d :bind
  "get_shadow_reverse_cull_face" :hash 36873697)
 bool)

(defgmethod
 (light-3d+set-shadow-caster-mask :class 'light-3d :bind
  "set_shadow_caster_mask" :hash 1286410249)
 :void (caster-mask int))

(defgmethod
 (light-3d+get-shadow-caster-mask :class 'light-3d :bind
  "get_shadow_caster_mask" :hash 3905245786)
 int)

(defgmethod
 (light-3d+set-bake-mode :class 'light-3d :bind "set_bake_mode" :hash 37739303)
 :void (bake-mode light-3d+bake-mode))

(defgmethod
 (light-3d+get-bake-mode :class 'light-3d :bind "get_bake_mode" :hash
  371737608)
 light-3d+bake-mode)

(defgmethod
 (light-3d+set-projector :class 'light-3d :bind "set_projector" :hash
  4051416890)
 :void (projector texture-2d))

(defgmethod
 (light-3d+get-projector :class 'light-3d :bind "get_projector" :hash
  3635182373)
 texture-2d)

(defgmethod
 (light-3d+set-temperature :class 'light-3d :bind "set_temperature" :hash
  373806689)
 :void (temperature float))

(defgmethod
 (light-3d+get-temperature :class 'light-3d :bind "get_temperature" :hash
  1740695150)
 float)

(defgmethod
 (light-3d+get-correlated-color :class 'light-3d :bind "get_correlated_color"
  :hash 3444240500)
 color)