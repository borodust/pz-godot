(common-lisp:in-package :%godot)


(defgmethod
 (path-2d+set-curve :class 'path-2d :bind "set_curve" :hash 659985499) :void
 (curve curve-2d))

(defgmethod
 (path-2d+get-curve :class 'path-2d :bind "get_curve" :hash 660369445) curve-2d)