(common-lisp:in-package :%godot)


(defgmethod (aabb+abs :class 'aabb :bind "abs" :hash 1576868580) aabb)

(defgmethod (aabb+get-center :class 'aabb :bind "get_center" :hash 1776574132)
 vector-3)

(defgmethod (aabb+get-volume :class 'aabb :bind "get_volume" :hash 466405837)
 float)

(defgmethod (aabb+has-volume :class 'aabb :bind "has_volume" :hash 3918633141)
 bool)

(defgmethod
 (aabb+has-surface :class 'aabb :bind "has_surface" :hash 3918633141) bool)

(defgmethod (aabb+has-point :class 'aabb :bind "has_point" :hash 1749054343)
 bool (point vector-3))

(defgmethod
 (aabb+is-equal-approx :class 'aabb :bind "is_equal_approx" :hash 299946684)
 bool (aabb aabb))

(defgmethod (aabb+is-finite :class 'aabb :bind "is_finite" :hash 3918633141)
 bool)

(defgmethod (aabb+intersects :class 'aabb :bind "intersects" :hash 299946684)
 bool (with aabb))

(defgmethod (aabb+encloses :class 'aabb :bind "encloses" :hash 299946684) bool
 (with aabb))

(defgmethod
 (aabb+intersects-plane :class 'aabb :bind "intersects_plane" :hash 1150170233)
 bool (plane plane))

(defgmethod
 (aabb+intersection :class 'aabb :bind "intersection" :hash 1271470306) aabb
 (with aabb))

(defgmethod (aabb+merge :class 'aabb :bind "merge" :hash 1271470306) aabb
 (with aabb))

(defgmethod (aabb+expand :class 'aabb :bind "expand" :hash 2851643018) aabb
 (to-point vector-3))

(defgmethod (aabb+grow :class 'aabb :bind "grow" :hash 239217291) aabb
 (by float))

(defgmethod
 (aabb+get-support :class 'aabb :bind "get_support" :hash 2923479887) vector-3
 (direction vector-3))

(defgmethod
 (aabb+get-longest-axis :class 'aabb :bind "get_longest_axis" :hash 1776574132)
 vector-3)

(defgmethod
 (aabb+get-longest-axis-index :class 'aabb :bind "get_longest_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (aabb+get-longest-axis-size :class 'aabb :bind "get_longest_axis_size" :hash
  466405837)
 float)

(defgmethod
 (aabb+get-shortest-axis :class 'aabb :bind "get_shortest_axis" :hash
  1776574132)
 vector-3)

(defgmethod
 (aabb+get-shortest-axis-index :class 'aabb :bind "get_shortest_axis_index"
  :hash 3173160232)
 int)

(defgmethod
 (aabb+get-shortest-axis-size :class 'aabb :bind "get_shortest_axis_size" :hash
  466405837)
 float)

(defgmethod
 (aabb+get-endpoint :class 'aabb :bind "get_endpoint" :hash 1394941017)
 vector-3 (idx int))

(defgmethod
 (aabb+intersects-segment :class 'aabb :bind "intersects_segment" :hash
  2048133369)
 variant (from vector-3) (to vector-3))

(defgmethod
 (aabb+intersects-ray :class 'aabb :bind "intersects_ray" :hash 2048133369)
 variant (from vector-3) (dir vector-3))