(common-lisp:in-package :%godot)


(defgmethod
 (vector-3i+min-axis-index :class 'vector-3i :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-3i+max-axis-index :class 'vector-3i :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-3i+distance-to :class 'vector-3i :bind "distance_to" :hash 1975170430)
 float (to vector-3i))

(defgmethod
 (vector-3i+distance-squared-to :class 'vector-3i :bind "distance_squared_to"
  :hash 2947717320)
 int (to vector-3i))

(defgmethod (vector-3i+length :class 'vector-3i :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-3i+length-squared :class 'vector-3i :bind "length_squared" :hash
  3173160232)
 int)

(defgmethod (vector-3i+sign :class 'vector-3i :bind "sign" :hash 3729604559)
 vector-3i)

(defgmethod (vector-3i+abs :class 'vector-3i :bind "abs" :hash 3729604559)
 vector-3i)

(defgmethod (vector-3i+clamp :class 'vector-3i :bind "clamp" :hash 1086892323)
 vector-3i (min vector-3i) (max vector-3i))

(defgmethod
 (vector-3i+clampi :class 'vector-3i :bind "clampi" :hash 1077216921) vector-3i
 (min int) (max int))

(defgmethod
 (vector-3i+snapped :class 'vector-3i :bind "snapped" :hash 1989319750)
 vector-3i (step vector-3i))

(defgmethod
 (vector-3i+snappedi :class 'vector-3i :bind "snappedi" :hash 2377625641)
 vector-3i (step int))

(defgmethod (vector-3i+min :class 'vector-3i :bind "min" :hash 1989319750)
 vector-3i (with vector-3i))

(defgmethod (vector-3i+mini :class 'vector-3i :bind "mini" :hash 2377625641)
 vector-3i (with int))

(defgmethod (vector-3i+max :class 'vector-3i :bind "max" :hash 1989319750)
 vector-3i (with vector-3i))

(defgmethod (vector-3i+maxi :class 'vector-3i :bind "maxi" :hash 2377625641)
 vector-3i (with int))