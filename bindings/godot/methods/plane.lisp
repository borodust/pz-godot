(common-lisp:in-package :%godot)


(defgmethod
 (plane+normalized :class 'plane :bind "normalized" :hash 1051796340) plane)

(defgmethod
 (plane+get-center :class 'plane :bind "get_center" :hash 1776574132) vector-3)

(defgmethod
 (plane+is-equal-approx :class 'plane :bind "is_equal_approx" :hash 1150170233)
 bool (to-plane plane))

(defgmethod (plane+is-finite :class 'plane :bind "is_finite" :hash 3918633141)
 bool)

(defgmethod
 (plane+is-point-over :class 'plane :bind "is_point_over" :hash 1749054343)
 bool (point vector-3))

(defgmethod
 (plane+distance-to :class 'plane :bind "distance_to" :hash 1047977935) float
 (point vector-3))

(defgmethod (plane+has-point :class 'plane :bind "has_point" :hash 1258189072)
 bool (point vector-3) (tolerance float))

(defgmethod (plane+project :class 'plane :bind "project" :hash 2923479887)
 vector-3 (point vector-3))

(defgmethod
 (plane+intersect-3 :class 'plane :bind "intersect_3" :hash 2012052692) variant
 (b plane) (c plane))

(defgmethod
 (plane+intersects-ray :class 'plane :bind "intersects_ray" :hash 2048133369)
 variant (from vector-3) (dir vector-3))

(defgmethod
 (plane+intersects-segment :class 'plane :bind "intersects_segment" :hash
  2048133369)
 variant (from vector-3) (to vector-3))