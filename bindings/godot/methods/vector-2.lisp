(common-lisp:in-package :%godot)


(defgmethod (vector-2+angle :class 'vector-2 :bind "angle" :hash 466405837)
 float)

(defgmethod
 (vector-2+angle-to :class 'vector-2 :bind "angle_to" :hash 3819070308) float
 (to vector-2))

(defgmethod
 (vector-2+angle-to-point :class 'vector-2 :bind "angle_to_point" :hash
  3819070308)
 float (to vector-2))

(defgmethod
 (vector-2+direction-to :class 'vector-2 :bind "direction_to" :hash 2026743667)
 vector-2 (to vector-2))

(defgmethod
 (vector-2+distance-to :class 'vector-2 :bind "distance_to" :hash 3819070308)
 float (to vector-2))

(defgmethod
 (vector-2+distance-squared-to :class 'vector-2 :bind "distance_squared_to"
  :hash 3819070308)
 float (to vector-2))

(defgmethod (vector-2+length :class 'vector-2 :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-2+length-squared :class 'vector-2 :bind "length_squared" :hash
  466405837)
 float)

(defgmethod
 (vector-2+limit-length :class 'vector-2 :bind "limit_length" :hash 2544004089)
 vector-2 (length float))

(defgmethod
 (vector-2+normalized :class 'vector-2 :bind "normalized" :hash 2428350749)
 vector-2)

(defgmethod
 (vector-2+is-normalized :class 'vector-2 :bind "is_normalized" :hash
  3918633141)
 bool)

(defgmethod
 (vector-2+is-equal-approx :class 'vector-2 :bind "is_equal_approx" :hash
  3190634762)
 bool (to vector-2))

(defgmethod
 (vector-2+is-zero-approx :class 'vector-2 :bind "is_zero_approx" :hash
  3918633141)
 bool)

(defgmethod
 (vector-2+is-finite :class 'vector-2 :bind "is_finite" :hash 3918633141) bool)

(defgmethod (vector-2+posmod :class 'vector-2 :bind "posmod" :hash 2544004089)
 vector-2 (mod float))

(defgmethod
 (vector-2+posmodv :class 'vector-2 :bind "posmodv" :hash 2026743667) vector-2
 (modv vector-2))

(defgmethod
 (vector-2+project :class 'vector-2 :bind "project" :hash 2026743667) vector-2
 (b vector-2))

(defgmethod (vector-2+lerp :class 'vector-2 :bind "lerp" :hash 4250033116)
 vector-2 (to vector-2) (weight float))

(defgmethod (vector-2+slerp :class 'vector-2 :bind "slerp" :hash 4250033116)
 vector-2 (to vector-2) (weight float))

(defgmethod
 (vector-2+cubic-interpolate :class 'vector-2 :bind "cubic_interpolate" :hash
  193522989)
 vector-2 (b vector-2) (pre-a vector-2) (post-b vector-2) (weight float))

(defgmethod
 (vector-2+cubic-interpolate-in-time :class 'vector-2 :bind
  "cubic_interpolate_in_time" :hash 1957055074)
 vector-2 (b vector-2) (pre-a vector-2) (post-b vector-2) (weight float)
 (b-t float) (pre-a-t float) (post-b-t float))

(defgmethod
 (vector-2+bezier-interpolate :class 'vector-2 :bind "bezier_interpolate" :hash
  193522989)
 vector-2 (control-1 vector-2) (control-2 vector-2) (end vector-2) (t float))

(defgmethod
 (vector-2+bezier-derivative :class 'vector-2 :bind "bezier_derivative" :hash
  193522989)
 vector-2 (control-1 vector-2) (control-2 vector-2) (end vector-2) (t float))

(defgmethod
 (vector-2+max-axis-index :class 'vector-2 :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-2+min-axis-index :class 'vector-2 :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-2+move-toward :class 'vector-2 :bind "move_toward" :hash 4250033116)
 vector-2 (to vector-2) (delta float))

(defgmethod
 (vector-2+rotated :class 'vector-2 :bind "rotated" :hash 2544004089) vector-2
 (angle float))

(defgmethod
 (vector-2+orthogonal :class 'vector-2 :bind "orthogonal" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+floor :class 'vector-2 :bind "floor" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+ceil :class 'vector-2 :bind "ceil" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+round :class 'vector-2 :bind "round" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+aspect :class 'vector-2 :bind "aspect" :hash 466405837)
 float)

(defgmethod (vector-2+dot :class 'vector-2 :bind "dot" :hash 3819070308) float
 (with vector-2))

(defgmethod (vector-2+slide :class 'vector-2 :bind "slide" :hash 2026743667)
 vector-2 (n vector-2))

(defgmethod (vector-2+bounce :class 'vector-2 :bind "bounce" :hash 2026743667)
 vector-2 (n vector-2))

(defgmethod
 (vector-2+reflect :class 'vector-2 :bind "reflect" :hash 2026743667) vector-2
 (line vector-2))

(defgmethod (vector-2+cross :class 'vector-2 :bind "cross" :hash 3819070308)
 float (with vector-2))

(defgmethod (vector-2+abs :class 'vector-2 :bind "abs" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+sign :class 'vector-2 :bind "sign" :hash 2428350749)
 vector-2)

(defgmethod (vector-2+clamp :class 'vector-2 :bind "clamp" :hash 318031021)
 vector-2 (min vector-2) (max vector-2))

(defgmethod (vector-2+clampf :class 'vector-2 :bind "clampf" :hash 3464402636)
 vector-2 (min float) (max float))

(defgmethod
 (vector-2+snapped :class 'vector-2 :bind "snapped" :hash 2026743667) vector-2
 (step vector-2))

(defgmethod
 (vector-2+snappedf :class 'vector-2 :bind "snappedf" :hash 2544004089)
 vector-2 (step float))

(defgmethod (vector-2+min :class 'vector-2 :bind "min" :hash 2026743667)
 vector-2 (with vector-2))

(defgmethod (vector-2+minf :class 'vector-2 :bind "minf" :hash 2544004089)
 vector-2 (with float))

(defgmethod (vector-2+max :class 'vector-2 :bind "max" :hash 2026743667)
 vector-2 (with vector-2))

(defgmethod (vector-2+maxf :class 'vector-2 :bind "maxf" :hash 2544004089)
 vector-2 (with float))

(defgmethod
 (vector-2+from-angle :class 'vector-2 :bind "from_angle" :hash 889263119
  :static common-lisp:t)
 vector-2 (angle float))