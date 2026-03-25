(common-lisp:in-package :%godot)


(defgmethod
 (open-xrcomposition-layer-quad+set-quad-size :class
  'open-xrcomposition-layer-quad :bind "set_quad_size" :hash 743155724)
 :void (size vector-2))

(defgmethod
 (open-xrcomposition-layer-quad+get-quad-size :class
  'open-xrcomposition-layer-quad :bind "get_quad_size" :hash 3341600327)
 vector-2)