(common-lisp:in-package :%godot)


(defgmethod
 (circle-shape-2d+set-radius :class 'circle-shape-2d :bind "set_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (circle-shape-2d+get-radius :class 'circle-shape-2d :bind "get_radius" :hash
  1740695150)
 float)