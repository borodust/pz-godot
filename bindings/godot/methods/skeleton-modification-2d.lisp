(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2d+-execute :class 'skeleton-modification-2d :bind
  "_execute" :hash 373806689 :virtual common-lisp:t)
 :void (delta float))

(defgmethod
 (skeleton-modification-2d+-setup-modification :class 'skeleton-modification-2d
  :bind "_setup_modification" :hash 3907307132 :virtual common-lisp:t)
 :void (modification-stack skeleton-modification-stack-2d))

(defgmethod
 (skeleton-modification-2d+-draw-editor-gizmo :class 'skeleton-modification-2d
  :bind "_draw_editor_gizmo" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (skeleton-modification-2d+set-enabled :class 'skeleton-modification-2d :bind
  "set_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (skeleton-modification-2d+get-enabled :class 'skeleton-modification-2d :bind
  "get_enabled" :hash 2240911060)
 bool)

(defgmethod
 (skeleton-modification-2d+get-modification-stack :class
  'skeleton-modification-2d :bind "get_modification_stack" :hash 2137761694)
 skeleton-modification-stack-2d)

(defgmethod
 (skeleton-modification-2d+set-is-setup :class 'skeleton-modification-2d :bind
  "set_is_setup" :hash 2586408642)
 :void (is-setup bool))

(defgmethod
 (skeleton-modification-2d+get-is-setup :class 'skeleton-modification-2d :bind
  "get_is_setup" :hash 36873697)
 bool)

(defgmethod
 (skeleton-modification-2d+set-execution-mode :class 'skeleton-modification-2d
  :bind "set_execution_mode" :hash 1286410249)
 :void (execution-mode int))

(defgmethod
 (skeleton-modification-2d+get-execution-mode :class 'skeleton-modification-2d
  :bind "get_execution_mode" :hash 3905245786)
 int)

(defgmethod
 (skeleton-modification-2d+clamp-angle :class 'skeleton-modification-2d :bind
  "clamp_angle" :hash 1229502682)
 float (angle float) (min float) (max float) (invert bool))

(defgmethod
 (skeleton-modification-2d+set-editor-draw-gizmo :class
  'skeleton-modification-2d :bind "set_editor_draw_gizmo" :hash 2586408642)
 :void (draw-gizmo bool))

(defgmethod
 (skeleton-modification-2d+get-editor-draw-gizmo :class
  'skeleton-modification-2d :bind "get_editor_draw_gizmo" :hash 36873697)
 bool)