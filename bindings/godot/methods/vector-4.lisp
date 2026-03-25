(common-lisp:in-package :%godot)


(defgmethod
 (vector-4+min-axis-index :class 'vector-4 :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-4+max-axis-index :class 'vector-4 :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod (vector-4+length :class 'vector-4 :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-4+length-squared :class 'vector-4 :bind "length_squared" :hash
  466405837)
 float)

(defgmethod (vector-4+abs :class 'vector-4 :bind "abs" :hash 80860099) vector-4)

(defgmethod (vector-4+sign :class 'vector-4 :bind "sign" :hash 80860099)
 vector-4)

(defgmethod (vector-4+floor :class 'vector-4 :bind "floor" :hash 80860099)
 vector-4)

(defgmethod (vector-4+ceil :class 'vector-4 :bind "ceil" :hash 80860099)
 vector-4)

(defgmethod (vector-4+round :class 'vector-4 :bind "round" :hash 80860099)
 vector-4)

(defgmethod (vector-4+lerp :class 'vector-4 :bind "lerp" :hash 2329757942)
 vector-4 (to vector-4) (weight float))

(defgmethod
 (vector-4+cubic-interpolate :class 'vector-4 :bind "cubic_interpolate" :hash
  726768410)
 vector-4 (b vector-4) (pre-a vector-4) (post-b vector-4) (weight float))

(defgmethod
 (vector-4+cubic-interpolate-in-time :class 'vector-4 :bind
  "cubic_interpolate_in_time" :hash 681631873)
 vector-4 (b vector-4) (pre-a vector-4) (post-b vector-4) (weight float)
 (b-t float) (pre-a-t float) (post-b-t float))

(defgmethod (vector-4+posmod :class 'vector-4 :bind "posmod" :hash 3129671720)
 vector-4 (mod float))

(defgmethod
 (vector-4+posmodv :class 'vector-4 :bind "posmodv" :hash 2031281584) vector-4
 (modv vector-4))

(defgmethod
 (vector-4+snapped :class 'vector-4 :bind "snapped" :hash 2031281584) vector-4
 (step vector-4))

(defgmethod
 (vector-4+snappedf :class 'vector-4 :bind "snappedf" :hash 3129671720)
 vector-4 (step float))

(defgmethod (vector-4+clamp :class 'vector-4 :bind "clamp" :hash 823915692)
 vector-4 (min vector-4) (max vector-4))

(defgmethod (vector-4+clampf :class 'vector-4 :bind "clampf" :hash 4072091586)
 vector-4 (min float) (max float))

(defgmethod
 (vector-4+normalized :class 'vector-4 :bind "normalized" :hash 80860099)
 vector-4)

(defgmethod
 (vector-4+is-normalized :class 'vector-4 :bind "is_normalized" :hash
  3918633141)
 bool)

(defgmethod
 (vector-4+direction-to :class 'vector-4 :bind "direction_to" :hash 2031281584)
 vector-4 (to vector-4))

(defgmethod
 (vector-4+distance-to :class 'vector-4 :bind "distance_to" :hash 3770801042)
 float (to vector-4))

(defgmethod
 (vector-4+distance-squared-to :class 'vector-4 :bind "distance_squared_to"
  :hash 3770801042)
 float (to vector-4))

(defgmethod (vector-4+dot :class 'vector-4 :bind "dot" :hash 3770801042) float
 (with vector-4))

(defgmethod (vector-4+inverse :class 'vector-4 :bind "inverse" :hash 80860099)
 vector-4)

(defgmethod
 (vector-4+is-equal-approx :class 'vector-4 :bind "is_equal_approx" :hash
  88913544)
 bool (to vector-4))

(defgmethod
 (vector-4+is-zero-approx :class 'vector-4 :bind "is_zero_approx" :hash
  3918633141)
 bool)

(defgmethod
 (vector-4+is-finite :class 'vector-4 :bind "is_finite" :hash 3918633141) bool)

(defgmethod (vector-4+min :class 'vector-4 :bind "min" :hash 2031281584)
 vector-4 (with vector-4))

(defgmethod (vector-4+minf :class 'vector-4 :bind "minf" :hash 3129671720)
 vector-4 (with float))

(defgmethod (vector-4+max :class 'vector-4 :bind "max" :hash 2031281584)
 vector-4 (with vector-4))

(defgmethod (vector-4+maxf :class 'vector-4 :bind "maxf" :hash 3129671720)
 vector-4 (with float))