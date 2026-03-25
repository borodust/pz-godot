(common-lisp:in-package :%godot)


(defgmethod
 (marker-2d+set-gizmo-extents :class 'marker-2d :bind "set_gizmo_extents" :hash
  373806689)
 :void (extents float))

(defgmethod
 (marker-2d+get-gizmo-extents :class 'marker-2d :bind "get_gizmo_extents" :hash
  1740695150)
 float)