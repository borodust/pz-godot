(common-lisp:in-package :%godot)


(defgmethod
 (directional-light-3d+set-shadow-mode :class 'directional-light-3d :bind
  "set_shadow_mode" :hash 1261211726)
 :void (mode directional-light-3d+shadow-mode))

(defgmethod
 (directional-light-3d+get-shadow-mode :class 'directional-light-3d :bind
  "get_shadow_mode" :hash 2765228544)
 directional-light-3d+shadow-mode)

(defgmethod
 (directional-light-3d+set-blend-splits :class 'directional-light-3d :bind
  "set_blend_splits" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (directional-light-3d+is-blend-splits-enabled :class 'directional-light-3d
  :bind "is_blend_splits_enabled" :hash 36873697)
 bool)

(defgmethod
 (directional-light-3d+set-sky-mode :class 'directional-light-3d :bind
  "set_sky_mode" :hash 2691194817)
 :void (mode directional-light-3d+sky-mode))

(defgmethod
 (directional-light-3d+get-sky-mode :class 'directional-light-3d :bind
  "get_sky_mode" :hash 3819982774)
 directional-light-3d+sky-mode)