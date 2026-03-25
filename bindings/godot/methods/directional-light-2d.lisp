(common-lisp:in-package :%godot)


(defgmethod
 (directional-light-2d+set-max-distance :class 'directional-light-2d :bind
  "set_max_distance" :hash 373806689)
 :void (pixels float))

(defgmethod
 (directional-light-2d+get-max-distance :class 'directional-light-2d :bind
  "get_max_distance" :hash 1740695150)
 float)