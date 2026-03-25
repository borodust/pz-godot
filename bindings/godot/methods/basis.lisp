(common-lisp:in-package :%godot)


(defgmethod (basis+inverse :class 'basis :bind "inverse" :hash 594669093) basis)

(defgmethod (basis+transposed :class 'basis :bind "transposed" :hash 594669093)
 basis)

(defgmethod
 (basis+orthonormalized :class 'basis :bind "orthonormalized" :hash 594669093)
 basis)

(defgmethod
 (basis+determinant :class 'basis :bind "determinant" :hash 466405837) float)

(defgmethod (basis+rotated :class 'basis :bind "rotated" :hash 1998708965)
 basis (axis vector-3) (angle float))

(defgmethod (basis+scaled :class 'basis :bind "scaled" :hash 3934786792) basis
 (scale vector-3))

(defgmethod
 (basis+scaled-local :class 'basis :bind "scaled_local" :hash 3934786792) basis
 (scale vector-3))

(defgmethod (basis+get-scale :class 'basis :bind "get_scale" :hash 1776574132)
 vector-3)

(defgmethod (basis+get-euler :class 'basis :bind "get_euler" :hash 1394941017)
 vector-3 (order int))

(defgmethod (basis+tdotx :class 'basis :bind "tdotx" :hash 1047977935) float
 (with vector-3))

(defgmethod (basis+tdoty :class 'basis :bind "tdoty" :hash 1047977935) float
 (with vector-3))

(defgmethod (basis+tdotz :class 'basis :bind "tdotz" :hash 1047977935) float
 (with vector-3))

(defgmethod (basis+slerp :class 'basis :bind "slerp" :hash 3118673011) basis
 (to basis) (weight float))

(defgmethod
 (basis+is-conformal :class 'basis :bind "is_conformal" :hash 3918633141) bool)

(defgmethod
 (basis+is-equal-approx :class 'basis :bind "is_equal_approx" :hash 3165333982)
 bool (b basis))

(defgmethod (basis+is-finite :class 'basis :bind "is_finite" :hash 3918633141)
 bool)

(defgmethod
 (basis+get-rotation-quaternion :class 'basis :bind "get_rotation_quaternion"
  :hash 4274879941)
 quaternion)

(defgmethod
 (basis+looking-at :class 'basis :bind "looking_at" :hash 3728732505 :static
  common-lisp:t)
 basis (target vector-3) (up vector-3) (use-model-front bool))

(defgmethod
 (basis+from-scale :class 'basis :bind "from_scale" :hash 3703240166 :static
  common-lisp:t)
 basis (scale vector-3))

(defgmethod
 (basis+from-euler :class 'basis :bind "from_euler" :hash 2802321791 :static
  common-lisp:t)
 basis (euler vector-3) (order int))