(common-lisp:in-package :%godot)


(defgmethod
 (sphere-occluder-3d+set-radius :class 'sphere-occluder-3d :bind "set_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (sphere-occluder-3d+get-radius :class 'sphere-occluder-3d :bind "get_radius"
  :hash 1740695150)
 float)