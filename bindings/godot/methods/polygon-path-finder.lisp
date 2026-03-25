(common-lisp:in-package :%godot)


(defgmethod
 (polygon-path-finder+setup :class 'polygon-path-finder :bind "setup" :hash
  3251786936)
 :void (points packed-vector-2array) (connections packed-int-32array))

(defgmethod
 (polygon-path-finder+find-path :class 'polygon-path-finder :bind "find_path"
  :hash 1562168077)
 packed-vector-2array (from vector-2) (to vector-2))

(defgmethod
 (polygon-path-finder+get-intersections :class 'polygon-path-finder :bind
  "get_intersections" :hash 3932192302)
 packed-vector-2array (from vector-2) (to vector-2))

(defgmethod
 (polygon-path-finder+get-closest-point :class 'polygon-path-finder :bind
  "get_closest_point" :hash 2656412154)
 vector-2 (point vector-2))

(defgmethod
 (polygon-path-finder+is-point-inside :class 'polygon-path-finder :bind
  "is_point_inside" :hash 556197845)
 bool (point vector-2))

(defgmethod
 (polygon-path-finder+set-point-penalty :class 'polygon-path-finder :bind
  "set_point_penalty" :hash 1602489585)
 :void (idx int) (penalty float))

(defgmethod
 (polygon-path-finder+get-point-penalty :class 'polygon-path-finder :bind
  "get_point_penalty" :hash 2339986948)
 float (idx int))

(defgmethod
 (polygon-path-finder+get-bounds :class 'polygon-path-finder :bind "get_bounds"
  :hash 1639390495)
 rect-2)