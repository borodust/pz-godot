(common-lisp:in-package :%godot)


(defgmethod
 (world-boundary-shape-2d+set-normal :class 'world-boundary-shape-2d :bind
  "set_normal" :hash 743155724)
 :void (normal vector-2))

(defgmethod
 (world-boundary-shape-2d+get-normal :class 'world-boundary-shape-2d :bind
  "get_normal" :hash 3341600327)
 vector-2)

(defgmethod
 (world-boundary-shape-2d+set-distance :class 'world-boundary-shape-2d :bind
  "set_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (world-boundary-shape-2d+get-distance :class 'world-boundary-shape-2d :bind
  "get_distance" :hash 1740695150)
 float)