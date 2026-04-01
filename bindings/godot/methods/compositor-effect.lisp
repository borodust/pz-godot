(common-lisp:in-package :%godot)


(defgmethod
 (compositor-effect+%render-callback :class 'compositor-effect :bind
  "_render_callback" :hash 2153422729 :virtual common-lisp:t)
 :void (effect-callback-type int) (render-data render-data))

(defgmethod
 (compositor-effect+set-enabled :class 'compositor-effect :bind "set_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (compositor-effect+get-enabled :class 'compositor-effect :bind "get_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (compositor-effect+set-effect-callback-type :class 'compositor-effect :bind
  "set_effect_callback_type" :hash 1390728419)
 :void (effect-callback-type compositor-effect+effect-callback-type))

(defgmethod
 (compositor-effect+get-effect-callback-type :class 'compositor-effect :bind
  "get_effect_callback_type" :hash 1221912590)
 compositor-effect+effect-callback-type)

(defgmethod
 (compositor-effect+set-access-resolved-color :class 'compositor-effect :bind
  "set_access_resolved_color" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (compositor-effect+get-access-resolved-color :class 'compositor-effect :bind
  "get_access_resolved_color" :hash 36873697)
 bool)

(defgmethod
 (compositor-effect+set-access-resolved-depth :class 'compositor-effect :bind
  "set_access_resolved_depth" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (compositor-effect+get-access-resolved-depth :class 'compositor-effect :bind
  "get_access_resolved_depth" :hash 36873697)
 bool)

(defgmethod
 (compositor-effect+set-needs-motion-vectors :class 'compositor-effect :bind
  "set_needs_motion_vectors" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (compositor-effect+get-needs-motion-vectors :class 'compositor-effect :bind
  "get_needs_motion_vectors" :hash 36873697)
 bool)

(defgmethod
 (compositor-effect+set-needs-normal-roughness :class 'compositor-effect :bind
  "set_needs_normal_roughness" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (compositor-effect+get-needs-normal-roughness :class 'compositor-effect :bind
  "get_needs_normal_roughness" :hash 36873697)
 bool)

(defgmethod
 (compositor-effect+set-needs-separate-specular :class 'compositor-effect :bind
  "set_needs_separate_specular" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (compositor-effect+get-needs-separate-specular :class 'compositor-effect :bind
  "get_needs_separate_specular" :hash 36873697)
 bool)