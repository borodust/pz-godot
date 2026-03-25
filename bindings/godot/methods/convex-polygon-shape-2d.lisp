(common-lisp:in-package :%godot)


(defgmethod
 (convex-polygon-shape-2d+set-point-cloud :class 'convex-polygon-shape-2d :bind
  "set_point_cloud" :hash 1509147220)
 :void (point-cloud packed-vector-2array))

(defgmethod
 (convex-polygon-shape-2d+set-points :class 'convex-polygon-shape-2d :bind
  "set_points" :hash 1509147220)
 :void (points packed-vector-2array))

(defgmethod
 (convex-polygon-shape-2d+get-points :class 'convex-polygon-shape-2d :bind
  "get_points" :hash 2961356807)
 packed-vector-2array)