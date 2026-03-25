(common-lisp:in-package :%godot)


(defgmethod
 (collision-polygon-3d+set-depth :class 'collision-polygon-3d :bind "set_depth"
  :hash 373806689)
 :void (depth float))

(defgmethod
 (collision-polygon-3d+get-depth :class 'collision-polygon-3d :bind "get_depth"
  :hash 1740695150)
 float)

(defgmethod
 (collision-polygon-3d+set-polygon :class 'collision-polygon-3d :bind
  "set_polygon" :hash 1509147220)
 :void (polygon packed-vector-2array))

(defgmethod
 (collision-polygon-3d+get-polygon :class 'collision-polygon-3d :bind
  "get_polygon" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (collision-polygon-3d+set-disabled :class 'collision-polygon-3d :bind
  "set_disabled" :hash 2586408642)
 :void (disabled bool))

(defgmethod
 (collision-polygon-3d+is-disabled :class 'collision-polygon-3d :bind
  "is_disabled" :hash 36873697)
 bool)

(defgmethod
 (collision-polygon-3d+set-debug-color :class 'collision-polygon-3d :bind
  "set_debug_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (collision-polygon-3d+get-debug-color :class 'collision-polygon-3d :bind
  "get_debug_color" :hash 3444240500)
 color)

(defgmethod
 (collision-polygon-3d+set-enable-debug-fill :class 'collision-polygon-3d :bind
  "set_enable_debug_fill" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (collision-polygon-3d+get-enable-debug-fill :class 'collision-polygon-3d :bind
  "get_enable_debug_fill" :hash 36873697)
 bool)

(defgmethod
 (collision-polygon-3d+set-margin :class 'collision-polygon-3d :bind
  "set_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (collision-polygon-3d+get-margin :class 'collision-polygon-3d :bind
  "get_margin" :hash 1740695150)
 float)