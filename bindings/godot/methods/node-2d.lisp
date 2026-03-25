(common-lisp:in-package :%godot)


(defgmethod
 (node-2d+set-position :class 'node-2d :bind "set_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (node-2d+set-rotation :class 'node-2d :bind "set_rotation" :hash 373806689)
 :void (radians float))

(defgmethod
 (node-2d+set-rotation-degrees :class 'node-2d :bind "set_rotation_degrees"
  :hash 373806689)
 :void (degrees float))

(defgmethod (node-2d+set-skew :class 'node-2d :bind "set_skew" :hash 373806689)
 :void (radians float))

(defgmethod
 (node-2d+set-scale :class 'node-2d :bind "set_scale" :hash 743155724) :void
 (scale vector-2))

(defgmethod
 (node-2d+get-position :class 'node-2d :bind "get_position" :hash 3341600327)
 vector-2)

(defgmethod
 (node-2d+get-rotation :class 'node-2d :bind "get_rotation" :hash 1740695150)
 float)

(defgmethod
 (node-2d+get-rotation-degrees :class 'node-2d :bind "get_rotation_degrees"
  :hash 1740695150)
 float)

(defgmethod
 (node-2d+get-skew :class 'node-2d :bind "get_skew" :hash 1740695150) float)

(defgmethod
 (node-2d+get-scale :class 'node-2d :bind "get_scale" :hash 3341600327)
 vector-2)

(defgmethod (node-2d+rotate :class 'node-2d :bind "rotate" :hash 373806689)
 :void (radians float))

(defgmethod
 (node-2d+move-local-x :class 'node-2d :bind "move_local_x" :hash 2087892650)
 :void (delta float) (scaled bool))

(defgmethod
 (node-2d+move-local-y :class 'node-2d :bind "move_local_y" :hash 2087892650)
 :void (delta float) (scaled bool))

(defgmethod
 (node-2d+translate :class 'node-2d :bind "translate" :hash 743155724) :void
 (offset vector-2))

(defgmethod
 (node-2d+global-translate :class 'node-2d :bind "global_translate" :hash
  743155724)
 :void (offset vector-2))

(defgmethod
 (node-2d+apply-scale :class 'node-2d :bind "apply_scale" :hash 743155724)
 :void (ratio vector-2))

(defgmethod
 (node-2d+set-global-position :class 'node-2d :bind "set_global_position" :hash
  743155724)
 :void (position vector-2))

(defgmethod
 (node-2d+get-global-position :class 'node-2d :bind "get_global_position" :hash
  3341600327)
 vector-2)

(defgmethod
 (node-2d+set-global-rotation :class 'node-2d :bind "set_global_rotation" :hash
  373806689)
 :void (radians float))

(defgmethod
 (node-2d+set-global-rotation-degrees :class 'node-2d :bind
  "set_global_rotation_degrees" :hash 373806689)
 :void (degrees float))

(defgmethod
 (node-2d+get-global-rotation :class 'node-2d :bind "get_global_rotation" :hash
  1740695150)
 float)

(defgmethod
 (node-2d+get-global-rotation-degrees :class 'node-2d :bind
  "get_global_rotation_degrees" :hash 1740695150)
 float)

(defgmethod
 (node-2d+set-global-skew :class 'node-2d :bind "set_global_skew" :hash
  373806689)
 :void (radians float))

(defgmethod
 (node-2d+get-global-skew :class 'node-2d :bind "get_global_skew" :hash
  1740695150)
 float)

(defgmethod
 (node-2d+set-global-scale :class 'node-2d :bind "set_global_scale" :hash
  743155724)
 :void (scale vector-2))

(defgmethod
 (node-2d+get-global-scale :class 'node-2d :bind "get_global_scale" :hash
  3341600327)
 vector-2)

(defgmethod
 (node-2d+set-transform :class 'node-2d :bind "set_transform" :hash 2761652528)
 :void (xform transform-2d))

(defgmethod
 (node-2d+set-global-transform :class 'node-2d :bind "set_global_transform"
  :hash 2761652528)
 :void (xform transform-2d))

(defgmethod (node-2d+look-at :class 'node-2d :bind "look_at" :hash 743155724)
 :void (point vector-2))

(defgmethod
 (node-2d+get-angle-to :class 'node-2d :bind "get_angle_to" :hash 2276447920)
 float (point vector-2))

(defgmethod
 (node-2d+to-local :class 'node-2d :bind "to_local" :hash 2656412154) vector-2
 (global-point vector-2))

(defgmethod
 (node-2d+to-global :class 'node-2d :bind "to_global" :hash 2656412154)
 vector-2 (local-point vector-2))

(defgmethod
 (node-2d+get-relative-transform-to-parent :class 'node-2d :bind
  "get_relative_transform_to_parent" :hash 904556875)
 transform-2d (parent node))