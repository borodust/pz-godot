(common-lisp:in-package :%godot)


(defgmethod
 (quaternion+length :class 'quaternion :bind "length" :hash 466405837) float)

(defgmethod
 (quaternion+length-squared :class 'quaternion :bind "length_squared" :hash
  466405837)
 float)

(defgmethod
 (quaternion+normalized :class 'quaternion :bind "normalized" :hash 4274879941)
 quaternion)

(defgmethod
 (quaternion+is-normalized :class 'quaternion :bind "is_normalized" :hash
  3918633141)
 bool)

(defgmethod
 (quaternion+is-equal-approx :class 'quaternion :bind "is_equal_approx" :hash
  1682156903)
 bool (to quaternion))

(defgmethod
 (quaternion+is-finite :class 'quaternion :bind "is_finite" :hash 3918633141)
 bool)

(defgmethod
 (quaternion+inverse :class 'quaternion :bind "inverse" :hash 4274879941)
 quaternion)

(defgmethod (quaternion+log :class 'quaternion :bind "log" :hash 4274879941)
 quaternion)

(defgmethod (quaternion+exp :class 'quaternion :bind "exp" :hash 4274879941)
 quaternion)

(defgmethod
 (quaternion+angle-to :class 'quaternion :bind "angle_to" :hash 3244682419)
 float (to quaternion))

(defgmethod (quaternion+dot :class 'quaternion :bind "dot" :hash 3244682419)
 float (with quaternion))

(defgmethod
 (quaternion+slerp :class 'quaternion :bind "slerp" :hash 1773590316)
 quaternion (to quaternion) (weight float))

(defgmethod
 (quaternion+slerpni :class 'quaternion :bind "slerpni" :hash 1773590316)
 quaternion (to quaternion) (weight float))

(defgmethod
 (quaternion+spherical-cubic-interpolate :class 'quaternion :bind
  "spherical_cubic_interpolate" :hash 2150967576)
 quaternion (b quaternion) (pre-a quaternion) (post-b quaternion)
 (weight float))

(defgmethod
 (quaternion+spherical-cubic-interpolate-in-time :class 'quaternion :bind
  "spherical_cubic_interpolate_in_time" :hash 1436023539)
 quaternion (b quaternion) (pre-a quaternion) (post-b quaternion)
 (weight float) (b-t float) (pre-a-t float) (post-b-t float))

(defgmethod
 (quaternion+get-euler :class 'quaternion :bind "get_euler" :hash 1394941017)
 vector-3 (order int))

(defgmethod
 (quaternion+from-euler :class 'quaternion :bind "from_euler" :hash 4053467903
  :static common-lisp:t)
 quaternion (euler vector-3))

(defgmethod
 (quaternion+get-axis :class 'quaternion :bind "get_axis" :hash 1776574132)
 vector-3)

(defgmethod
 (quaternion+get-angle :class 'quaternion :bind "get_angle" :hash 466405837)
 float)