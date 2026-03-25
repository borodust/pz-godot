(common-lisp:in-package :%godot)


(defgmethod
 (sphere-shape-3d+set-radius :class 'sphere-shape-3d :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (sphere-shape-3d+get-radius :class 'sphere-shape-3d :bind "get_radius" :hash
  1740695150)
 float)