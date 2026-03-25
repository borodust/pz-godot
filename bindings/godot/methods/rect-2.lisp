(common-lisp:in-package :%godot)


(defgmethod
 (rect-2+get-center :class 'rect-2 :bind "get_center" :hash 2428350749)
 vector-2)

(defgmethod (rect-2+get-area :class 'rect-2 :bind "get_area" :hash 466405837)
 float)

(defgmethod (rect-2+has-area :class 'rect-2 :bind "has_area" :hash 3918633141)
 bool)

(defgmethod
 (rect-2+has-point :class 'rect-2 :bind "has_point" :hash 3190634762) bool
 (point vector-2))

(defgmethod
 (rect-2+is-equal-approx :class 'rect-2 :bind "is_equal_approx" :hash
  1908192260)
 bool (rect rect-2))

(defgmethod
 (rect-2+is-finite :class 'rect-2 :bind "is_finite" :hash 3918633141) bool)

(defgmethod
 (rect-2+intersects :class 'rect-2 :bind "intersects" :hash 819294880) bool
 (b rect-2) (include-borders bool))

(defgmethod (rect-2+encloses :class 'rect-2 :bind "encloses" :hash 1908192260)
 bool (b rect-2))

(defgmethod
 (rect-2+intersection :class 'rect-2 :bind "intersection" :hash 2282977743)
 rect-2 (b rect-2))

(defgmethod (rect-2+merge :class 'rect-2 :bind "merge" :hash 2282977743) rect-2
 (b rect-2))

(defgmethod (rect-2+expand :class 'rect-2 :bind "expand" :hash 293272265)
 rect-2 (to vector-2))

(defgmethod
 (rect-2+get-support :class 'rect-2 :bind "get_support" :hash 2026743667)
 vector-2 (direction vector-2))

(defgmethod (rect-2+grow :class 'rect-2 :bind "grow" :hash 39664498) rect-2
 (amount float))

(defgmethod
 (rect-2+grow-side :class 'rect-2 :bind "grow_side" :hash 4177736158) rect-2
 (side int) (amount float))

(defgmethod
 (rect-2+grow-individual :class 'rect-2 :bind "grow_individual" :hash
  3203390369)
 rect-2 (left float) (top float) (right float) (bottom float))

(defgmethod (rect-2+abs :class 'rect-2 :bind "abs" :hash 3107653634) rect-2)