(common-lisp:in-package :%godot)


(defgmethod (vector-2i+aspect :class 'vector-2i :bind "aspect" :hash 466405837)
 float)

(defgmethod
 (vector-2i+max-axis-index :class 'vector-2i :bind "max_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-2i+min-axis-index :class 'vector-2i :bind "min_axis_index" :hash
  3173160232)
 int)

(defgmethod
 (vector-2i+distance-to :class 'vector-2i :bind "distance_to" :hash 707501214)
 float (to vector-2i))

(defgmethod
 (vector-2i+distance-squared-to :class 'vector-2i :bind "distance_squared_to"
  :hash 1130029528)
 int (to vector-2i))

(defgmethod (vector-2i+length :class 'vector-2i :bind "length" :hash 466405837)
 float)

(defgmethod
 (vector-2i+length-squared :class 'vector-2i :bind "length_squared" :hash
  3173160232)
 int)

(defgmethod (vector-2i+sign :class 'vector-2i :bind "sign" :hash 3444277866)
 vector-2i)

(defgmethod (vector-2i+abs :class 'vector-2i :bind "abs" :hash 3444277866)
 vector-2i)

(defgmethod (vector-2i+clamp :class 'vector-2i :bind "clamp" :hash 186568249)
 vector-2i (min vector-2i) (max vector-2i))

(defgmethod
 (vector-2i+clampi :class 'vector-2i :bind "clampi" :hash 3686769569) vector-2i
 (min int) (max int))

(defgmethod
 (vector-2i+snapped :class 'vector-2i :bind "snapped" :hash 1735278196)
 vector-2i (step vector-2i))

(defgmethod
 (vector-2i+snappedi :class 'vector-2i :bind "snappedi" :hash 2161988953)
 vector-2i (step int))

(defgmethod (vector-2i+min :class 'vector-2i :bind "min" :hash 1735278196)
 vector-2i (with vector-2i))

(defgmethod (vector-2i+mini :class 'vector-2i :bind "mini" :hash 2161988953)
 vector-2i (with int))

(defgmethod (vector-2i+max :class 'vector-2i :bind "max" :hash 1735278196)
 vector-2i (with vector-2i))

(defgmethod (vector-2i+maxi :class 'vector-2i :bind "maxi" :hash 2161988953)
 vector-2i (with int))