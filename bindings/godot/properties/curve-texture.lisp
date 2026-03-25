(common-lisp:in-package :%godot)


(defgproperty curve-texture+width 'curve-texture :set 'curve-texture+set-width)

(defgproperty curve-texture+texture-mode 'curve-texture :get
 'curve-texture+get-texture-mode :set 'curve-texture+set-texture-mode)

(defgproperty curve-texture+curve 'curve-texture :get 'curve-texture+get-curve
 :set 'curve-texture+set-curve)