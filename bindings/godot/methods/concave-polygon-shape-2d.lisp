(common-lisp:in-package :%godot)


(defgmethod
 (concave-polygon-shape-2d+set-segments :class 'concave-polygon-shape-2d :bind
  "set_segments" :hash 1509147220)
 :void (segments packed-vector-2array))

(defgmethod
 (concave-polygon-shape-2d+get-segments :class 'concave-polygon-shape-2d :bind
  "get_segments" :hash 2961356807)
 packed-vector-2array)