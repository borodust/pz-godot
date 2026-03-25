(common-lisp:in-package :%godot)


(defgmethod
 (cylinder-shape-3d+set-radius :class 'cylinder-shape-3d :bind "set_radius"
  :hash 373806689)
 :void (radius float))

(defgmethod
 (cylinder-shape-3d+get-radius :class 'cylinder-shape-3d :bind "get_radius"
  :hash 1740695150)
 float)

(defgmethod
 (cylinder-shape-3d+set-height :class 'cylinder-shape-3d :bind "set_height"
  :hash 373806689)
 :void (height float))

(defgmethod
 (cylinder-shape-3d+get-height :class 'cylinder-shape-3d :bind "get_height"
  :hash 1740695150)
 float)