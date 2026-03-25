(common-lisp:in-package :%godot)


(defgmethod
 (polygon-occluder-3d+set-polygon :class 'polygon-occluder-3d :bind
  "set_polygon" :hash 1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (polygon-occluder-3d+get-polygon :class 'polygon-occluder-3d :bind
  "get_polygon" :hash 2961356807)
 packed-vector-2array)