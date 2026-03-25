(common-lisp:in-package :%godot)


(defgmethod
 (array-occluder-3d+set-arrays :class 'array-occluder-3d :bind "set_arrays"
  :hash 3233972621)
 :void (vertices packed-vector-3array) (indices packed-int-32array))

(defgmethod
 (array-occluder-3d+set-vertices :class 'array-occluder-3d :bind "set_vertices"
  :hash 334873810)
 :void (vertices packed-vector-3array))

(defgmethod
 (array-occluder-3d+set-indices :class 'array-occluder-3d :bind "set_indices"
  :hash 3614634198)
 :void (indices packed-int-32array))