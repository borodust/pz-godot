(common-lisp:in-package :%godot)


(defgmethod
 (path-3d+set-curve :class 'path-3d :bind "set_curve" :hash 408955118) :void
 (curve curve-3d))

(defgmethod
 (path-3d+get-curve :class 'path-3d :bind "get_curve" :hash 4244715212)
 curve-3d)

(defgmethod
 (path-3d+set-debug-custom-color :class 'path-3d :bind "set_debug_custom_color"
  :hash 2920490490)
 :void (debug-custom-color color))

(defgmethod
 (path-3d+get-debug-custom-color :class 'path-3d :bind "get_debug_custom_color"
  :hash 3444240500)
 color)