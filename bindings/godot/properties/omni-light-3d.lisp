(common-lisp:in-package :%godot)


(defgproperty omni-light-3d+omni-range 'omni-light-3d :index 4)

(defgproperty omni-light-3d+omni-attenuation 'omni-light-3d :index 6)

(defgproperty omni-light-3d+omni-shadow-mode 'omni-light-3d :get
 'omni-light-3d+get-shadow-mode :set 'omni-light-3d+set-shadow-mode)