(common-lisp:in-package :%godot)


(defgmethod
 (convex-polygon-shape-3d+set-points :class 'convex-polygon-shape-3d :bind
  "set_points" :hash 334873810)
 :void (points packed-vector-3array))

(defgmethod
 (convex-polygon-shape-3d+get-points :class 'convex-polygon-shape-3d :bind
  "get_points" :hash 497664490)
 packed-vector-3array)