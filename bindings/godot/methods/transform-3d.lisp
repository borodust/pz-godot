(common-lisp:in-package :%godot)


(defgmethod
 (transform-3d+inverse :class 'transform-3d :bind "inverse" :hash 3816817146)
 transform-3d)

(defgmethod
 (transform-3d+affine-inverse :class 'transform-3d :bind "affine_inverse" :hash
  3816817146)
 transform-3d)

(defgmethod
 (transform-3d+orthonormalized :class 'transform-3d :bind "orthonormalized"
  :hash 3816817146)
 transform-3d)

(defgmethod
 (transform-3d+rotated :class 'transform-3d :bind "rotated" :hash 1563203923)
 transform-3d (axis vector-3) (angle float))

(defgmethod
 (transform-3d+rotated-local :class 'transform-3d :bind "rotated_local" :hash
  1563203923)
 transform-3d (axis vector-3) (angle float))

(defgmethod
 (transform-3d+scaled :class 'transform-3d :bind "scaled" :hash 1405596198)
 transform-3d (scale vector-3))

(defgmethod
 (transform-3d+scaled-local :class 'transform-3d :bind "scaled_local" :hash
  1405596198)
 transform-3d (scale vector-3))

(defgmethod
 (transform-3d+translated :class 'transform-3d :bind "translated" :hash
  1405596198)
 transform-3d (offset vector-3))

(defgmethod
 (transform-3d+translated-local :class 'transform-3d :bind "translated_local"
  :hash 1405596198)
 transform-3d (offset vector-3))

(defgmethod
 (transform-3d+looking-at :class 'transform-3d :bind "looking_at" :hash
  90889270)
 transform-3d (target vector-3) (up vector-3) (use-model-front bool))

(defgmethod
 (transform-3d+interpolate-with :class 'transform-3d :bind "interpolate_with"
  :hash 1786453358)
 transform-3d (xform transform-3d) (weight float))

(defgmethod
 (transform-3d+is-equal-approx :class 'transform-3d :bind "is_equal_approx"
  :hash 696001652)
 bool (xform transform-3d))

(defgmethod
 (transform-3d+is-finite :class 'transform-3d :bind "is_finite" :hash
  3918633141)
 bool)