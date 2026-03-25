(common-lisp:in-package :%godot)


(defgmethod
 (rect-2i+get-center :class 'rect-2i :bind "get_center" :hash 3444277866)
 vector-2i)

(defgmethod
 (rect-2i+get-area :class 'rect-2i :bind "get_area" :hash 3173160232) int)

(defgmethod
 (rect-2i+has-area :class 'rect-2i :bind "has_area" :hash 3918633141) bool)

(defgmethod
 (rect-2i+has-point :class 'rect-2i :bind "has_point" :hash 328189994) bool
 (point vector-2i))

(defgmethod
 (rect-2i+intersects :class 'rect-2i :bind "intersects" :hash 3434691493) bool
 (b rect-2i))

(defgmethod
 (rect-2i+encloses :class 'rect-2i :bind "encloses" :hash 3434691493) bool
 (b rect-2i))

(defgmethod
 (rect-2i+intersection :class 'rect-2i :bind "intersection" :hash 717431873)
 rect-2i (b rect-2i))

(defgmethod (rect-2i+merge :class 'rect-2i :bind "merge" :hash 717431873)
 rect-2i (b rect-2i))

(defgmethod (rect-2i+expand :class 'rect-2i :bind "expand" :hash 1355196872)
 rect-2i (to vector-2i))

(defgmethod (rect-2i+grow :class 'rect-2i :bind "grow" :hash 1578070074)
 rect-2i (amount int))

(defgmethod
 (rect-2i+grow-side :class 'rect-2i :bind "grow_side" :hash 3191154199) rect-2i
 (side int) (amount int))

(defgmethod
 (rect-2i+grow-individual :class 'rect-2i :bind "grow_individual" :hash
  1893743416)
 rect-2i (left int) (top int) (right int) (bottom int))

(defgmethod (rect-2i+abs :class 'rect-2i :bind "abs" :hash 1469025700) rect-2i)