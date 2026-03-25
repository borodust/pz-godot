(common-lisp:in-package :%godot)


(defgmethod
 (animatable-body-3d+set-sync-to-physics :class 'animatable-body-3d :bind
  "set_sync_to_physics" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animatable-body-3d+is-sync-to-physics-enabled :class 'animatable-body-3d
  :bind "is_sync_to_physics_enabled" :hash 36873697)
 bool)