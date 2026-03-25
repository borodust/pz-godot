(common-lisp:in-package :%godot)


(defgmethod
 (occluder-polygon-2d+set-closed :class 'occluder-polygon-2d :bind "set_closed"
  :hash 2586408642)
 :void (closed bool))

(defgmethod
 (occluder-polygon-2d+is-closed :class 'occluder-polygon-2d :bind "is_closed"
  :hash 36873697)
 bool)

(defgmethod
 (occluder-polygon-2d+set-cull-mode :class 'occluder-polygon-2d :bind
  "set_cull_mode" :hash 3500863002)
 :void (cull-mode occluder-polygon-2d+cull-mode))

(defgmethod
 (occluder-polygon-2d+get-cull-mode :class 'occluder-polygon-2d :bind
  "get_cull_mode" :hash 33931036)
 occluder-polygon-2d+cull-mode)

(defgmethod
 (occluder-polygon-2d+set-polygon :class 'occluder-polygon-2d :bind
  "set_polygon" :hash 1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (occluder-polygon-2d+get-polygon :class 'occluder-polygon-2d :bind
  "get_polygon" :hash 2961356807)
 packed-vector-2array)