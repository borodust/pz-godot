(common-lisp:in-package :%godot)


(defgmethod
 (open-xrspatial-component-mesh-2dlist+get-transform :class
  'open-xrspatial-component-mesh-2dlist :bind "get_transform" :hash 1965739696)
 transform-3d (index int))

(defgmethod
 (open-xrspatial-component-mesh-2dlist+get-vertices :class
  'open-xrspatial-component-mesh-2dlist :bind "get_vertices" :hash 110850971)
 packed-vector-2array (snapshot rid) (index int))

(defgmethod
 (open-xrspatial-component-mesh-2dlist+get-indices :class
  'open-xrspatial-component-mesh-2dlist :bind "get_indices" :hash 3393655756)
 packed-int-32array (snapshot rid) (index int))