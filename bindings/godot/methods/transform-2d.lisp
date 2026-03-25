(common-lisp:in-package :%godot)


(defgmethod
 (transform-2d+inverse :class 'transform-2d :bind "inverse" :hash 1420440541)
 transform-2d)

(defgmethod
 (transform-2d+affine-inverse :class 'transform-2d :bind "affine_inverse" :hash
  1420440541)
 transform-2d)

(defgmethod
 (transform-2d+get-rotation :class 'transform-2d :bind "get_rotation" :hash
  466405837)
 float)

(defgmethod
 (transform-2d+get-origin :class 'transform-2d :bind "get_origin" :hash
  2428350749)
 vector-2)

(defgmethod
 (transform-2d+get-scale :class 'transform-2d :bind "get_scale" :hash
  2428350749)
 vector-2)

(defgmethod
 (transform-2d+get-skew :class 'transform-2d :bind "get_skew" :hash 466405837)
 float)

(defgmethod
 (transform-2d+orthonormalized :class 'transform-2d :bind "orthonormalized"
  :hash 1420440541)
 transform-2d)

(defgmethod
 (transform-2d+rotated :class 'transform-2d :bind "rotated" :hash 729597514)
 transform-2d (angle float))

(defgmethod
 (transform-2d+rotated-local :class 'transform-2d :bind "rotated_local" :hash
  729597514)
 transform-2d (angle float))

(defgmethod
 (transform-2d+scaled :class 'transform-2d :bind "scaled" :hash 1446323263)
 transform-2d (scale vector-2))

(defgmethod
 (transform-2d+scaled-local :class 'transform-2d :bind "scaled_local" :hash
  1446323263)
 transform-2d (scale vector-2))

(defgmethod
 (transform-2d+translated :class 'transform-2d :bind "translated" :hash
  1446323263)
 transform-2d (offset vector-2))

(defgmethod
 (transform-2d+translated-local :class 'transform-2d :bind "translated_local"
  :hash 1446323263)
 transform-2d (offset vector-2))

(defgmethod
 (transform-2d+determinant :class 'transform-2d :bind "determinant" :hash
  466405837)
 float)

(defgmethod
 (transform-2d+basis-xform :class 'transform-2d :bind "basis_xform" :hash
  2026743667)
 vector-2 (v vector-2))

(defgmethod
 (transform-2d+basis-xform-inv :class 'transform-2d :bind "basis_xform_inv"
  :hash 2026743667)
 vector-2 (v vector-2))

(defgmethod
 (transform-2d+interpolate-with :class 'transform-2d :bind "interpolate_with"
  :hash 359399686)
 transform-2d (xform transform-2d) (weight float))

(defgmethod
 (transform-2d+is-conformal :class 'transform-2d :bind "is_conformal" :hash
  3918633141)
 bool)

(defgmethod
 (transform-2d+is-equal-approx :class 'transform-2d :bind "is_equal_approx"
  :hash 3837431929)
 bool (xform transform-2d))

(defgmethod
 (transform-2d+is-finite :class 'transform-2d :bind "is_finite" :hash
  3918633141)
 bool)

(defgmethod
 (transform-2d+looking-at :class 'transform-2d :bind "looking_at" :hash
  1446323263)
 transform-2d (target vector-2))