(common-lisp:in-package :%godot)


(defgmethod
 (marker-3d+set-gizmo-extents :class 'marker-3d :bind "set_gizmo_extents" :hash
  373806689)
 :void (extents float))

(defgmethod
 (marker-3d+get-gizmo-extents :class 'marker-3d :bind "get_gizmo_extents" :hash
  1740695150)
 float)