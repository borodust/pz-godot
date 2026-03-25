(common-lisp:in-package :%godot)


(defgmethod
 (triangle-mesh+create-from-faces :class 'triangle-mesh :bind
  "create_from_faces" :hash 2637816732)
 bool (faces packed-vector-3array))

(defgmethod
 (triangle-mesh+get-faces :class 'triangle-mesh :bind "get_faces" :hash
  497664490)
 packed-vector-3array)

(defgmethod
 (triangle-mesh+intersect-segment :class 'triangle-mesh :bind
  "intersect_segment" :hash 3648293151)
 dictionary (begin vector-3) (end vector-3))

(defgmethod
 (triangle-mesh+intersect-ray :class 'triangle-mesh :bind "intersect_ray" :hash
  3648293151)
 dictionary (begin vector-3) (dir vector-3))