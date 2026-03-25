(common-lisp:in-package :%godot)


(defgmethod
 (xrorigin-3d+set-world-scale :class 'xrorigin-3d :bind "set_world_scale" :hash
  373806689)
 :void (world-scale float))

(defgmethod
 (xrorigin-3d+get-world-scale :class 'xrorigin-3d :bind "get_world_scale" :hash
  1740695150)
 float)

(defgmethod
 (xrorigin-3d+set-current :class 'xrorigin-3d :bind "set_current" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (xrorigin-3d+is-current :class 'xrorigin-3d :bind "is_current" :hash 36873697)
 bool)