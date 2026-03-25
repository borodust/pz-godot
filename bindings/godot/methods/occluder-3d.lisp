(common-lisp:in-package :%godot)


(defgmethod
 (occluder-3d+get-vertices :class 'occluder-3d :bind "get_vertices" :hash
  497664490)
 packed-vector-3array)

(defgmethod
 (occluder-3d+get-indices :class 'occluder-3d :bind "get_indices" :hash
  1930428628)
 packed-int-32array)