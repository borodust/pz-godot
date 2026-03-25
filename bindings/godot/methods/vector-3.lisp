(common-lisp:in-package :%godot)


(defgmethod
 (vector-3+min-axis-index :class 'vector-3 :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-3+max-axis-index :class 'vector-3 :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-3+angle-to :class 'vector-3 :bind "angle_to" :hash 1047977935) float
 (to vector-3))

(defgmethod
 (vector-3+signed-angle-to :class 'vector-3 :bind "signed_angle_to" :hash
  2781412522)
 float (to vector-3) (axis vector-3))

(defgmethod
 (vector-3+direction-to :class 'vector-3 :bind "direction_to" :hash 2923479887)
 vector-3 (to vector-3))

(defgmethod
 (vector-3+distance-to :class 'vector-3 :bind "distance_to" :hash 1047977935)
 float (to vector-3))

(defgmethod
 (vector-3+distance-squared-to :class 'vector-3 :bind "distance_squared_to"
  :hash 1047977935)
 float (to vector-3))

(defgmethod (vector-3+length :class 'vector-3 :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-3+length-squared :class 'vector-3 :bind "length_squared" :hash
  466405837)
 float)

(defgmethod
 (vector-3+limit-length :class 'vector-3 :bind "limit_length" :hash 514930144)
 vector-3 (length float))

(defgmethod
 (vector-3+normalized :class 'vector-3 :bind "normalized" :hash 1776574132)
 vector-3)

(defgmethod
 (vector-3+is-normalized :class 'vector-3 :bind "is_normalized" :hash
  3918633141)
 bool)

(defgmethod
 (vector-3+is-equal-approx :class 'vector-3 :bind "is_equal_approx" :hash
  1749054343)
 bool (to vector-3))

(defgmethod
 (vector-3+is-zero-approx :class 'vector-3 :bind "is_zero_approx" :hash
  3918633141)
 bool)

(defgmethod
 (vector-3+is-finite :class 'vector-3 :bind "is_finite" :hash 3918633141) bool)

(defgmethod
 (vector-3+inverse :class 'vector-3 :bind "inverse" :hash 1776574132) vector-3)

(defgmethod (vector-3+clamp :class 'vector-3 :bind "clamp" :hash 4145107892)
 vector-3 (min vector-3) (max vector-3))

(defgmethod (vector-3+clampf :class 'vector-3 :bind "clampf" :hash 2329594628)
 vector-3 (min float) (max float))

(defgmethod
 (vector-3+snapped :class 'vector-3 :bind "snapped" :hash 2923479887) vector-3
 (step vector-3))

(defgmethod
 (vector-3+snappedf :class 'vector-3 :bind "snappedf" :hash 514930144) vector-3
 (step float))

(defgmethod
 (vector-3+rotated :class 'vector-3 :bind "rotated" :hash 1682608829) vector-3
 (axis vector-3) (angle float))

(defgmethod (vector-3+lerp :class 'vector-3 :bind "lerp" :hash 1682608829)
 vector-3 (to vector-3) (weight float))

(defgmethod (vector-3+slerp :class 'vector-3 :bind "slerp" :hash 1682608829)
 vector-3 (to vector-3) (weight float))

(defgmethod
 (vector-3+cubic-interpolate :class 'vector-3 :bind "cubic_interpolate" :hash
  2597922253)
 vector-3 (b vector-3) (pre-a vector-3) (post-b vector-3) (weight float))

(defgmethod
 (vector-3+cubic-interpolate-in-time :class 'vector-3 :bind
  "cubic_interpolate_in_time" :hash 3256682901)
 vector-3 (b vector-3) (pre-a vector-3) (post-b vector-3) (weight float)
 (b-t float) (pre-a-t float) (post-b-t float))

(defgmethod
 (vector-3+bezier-interpolate :class 'vector-3 :bind "bezier_interpolate" :hash
  2597922253)
 vector-3 (control-1 vector-3) (control-2 vector-3) (end vector-3) (t float))

(defgmethod
 (vector-3+bezier-derivative :class 'vector-3 :bind "bezier_derivative" :hash
  2597922253)
 vector-3 (control-1 vector-3) (control-2 vector-3) (end vector-3) (t float))

(defgmethod
 (vector-3+move-toward :class 'vector-3 :bind "move_toward" :hash 1682608829)
 vector-3 (to vector-3) (delta float))

(defgmethod (vector-3+dot :class 'vector-3 :bind "dot" :hash 1047977935) float
 (with vector-3))

(defgmethod (vector-3+cross :class 'vector-3 :bind "cross" :hash 2923479887)
 vector-3 (with vector-3))

(defgmethod (vector-3+outer :class 'vector-3 :bind "outer" :hash 3934786792)
 basis (with vector-3))

(defgmethod (vector-3+abs :class 'vector-3 :bind "abs" :hash 1776574132)
 vector-3)

(defgmethod (vector-3+floor :class 'vector-3 :bind "floor" :hash 1776574132)
 vector-3)

(defgmethod (vector-3+ceil :class 'vector-3 :bind "ceil" :hash 1776574132)
 vector-3)

(defgmethod (vector-3+round :class 'vector-3 :bind "round" :hash 1776574132)
 vector-3)

(defgmethod (vector-3+posmod :class 'vector-3 :bind "posmod" :hash 514930144)
 vector-3 (mod float))

(defgmethod
 (vector-3+posmodv :class 'vector-3 :bind "posmodv" :hash 2923479887) vector-3
 (modv vector-3))

(defgmethod
 (vector-3+project :class 'vector-3 :bind "project" :hash 2923479887) vector-3
 (b vector-3))

(defgmethod (vector-3+slide :class 'vector-3 :bind "slide" :hash 2923479887)
 vector-3 (n vector-3))

(defgmethod (vector-3+bounce :class 'vector-3 :bind "bounce" :hash 2923479887)
 vector-3 (n vector-3))

(defgmethod
 (vector-3+reflect :class 'vector-3 :bind "reflect" :hash 2923479887) vector-3
 (n vector-3))

(defgmethod (vector-3+sign :class 'vector-3 :bind "sign" :hash 1776574132)
 vector-3)

(defgmethod
 (vector-3+octahedron-encode :class 'vector-3 :bind "octahedron_encode" :hash
  2428350749)
 vector-2)

(defgmethod (vector-3+min :class 'vector-3 :bind "min" :hash 2923479887)
 vector-3 (with vector-3))

(defgmethod (vector-3+minf :class 'vector-3 :bind "minf" :hash 514930144)
 vector-3 (with float))

(defgmethod (vector-3+max :class 'vector-3 :bind "max" :hash 2923479887)
 vector-3 (with vector-3))

(defgmethod (vector-3+maxf :class 'vector-3 :bind "maxf" :hash 514930144)
 vector-3 (with float))

(defgmethod
 (vector-3+octahedron-decode :class 'vector-3 :bind "octahedron_decode" :hash
  3991820552 :static common-lisp:t)
 vector-3 (uv vector-2))