(common-lisp:in-package :%godot)


(defgmethod
 (vector-4i+min-axis-index :class 'vector-4i :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-4i+max-axis-index :class 'vector-4i :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod (vector-4i+length :class 'vector-4i :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-4i+length-squared :class 'vector-4i :bind "length_squared" :hash
  3173160232)
 int)

(defgmethod (vector-4i+sign :class 'vector-4i :bind "sign" :hash 4134919947)
 vector-4i)

(defgmethod (vector-4i+abs :class 'vector-4i :bind "abs" :hash 4134919947)
 vector-4i)

(defgmethod (vector-4i+clamp :class 'vector-4i :bind "clamp" :hash 3046490913)
 vector-4i (min vector-4i) (max vector-4i))

(defgmethod
 (vector-4i+clampi :class 'vector-4i :bind "clampi" :hash 2994578256) vector-4i
 (min int) (max int))

(defgmethod
 (vector-4i+snapped :class 'vector-4i :bind "snapped" :hash 1181693102)
 vector-4i (step vector-4i))

(defgmethod
 (vector-4i+snappedi :class 'vector-4i :bind "snappedi" :hash 1476494415)
 vector-4i (step int))

(defgmethod (vector-4i+min :class 'vector-4i :bind "min" :hash 1181693102)
 vector-4i (with vector-4i))

(defgmethod (vector-4i+mini :class 'vector-4i :bind "mini" :hash 1476494415)
 vector-4i (with int))

(defgmethod (vector-4i+max :class 'vector-4i :bind "max" :hash 1181693102)
 vector-4i (with vector-4i))

(defgmethod (vector-4i+maxi :class 'vector-4i :bind "maxi" :hash 1476494415)
 vector-4i (with int))

(defgmethod
 (vector-4i+distance-to :class 'vector-4i :bind "distance_to" :hash 3446086573)
 float (to vector-4i))

(defgmethod
 (vector-4i+distance-squared-to :class 'vector-4i :bind "distance_squared_to"
  :hash 346708794)
 int (to vector-4i))