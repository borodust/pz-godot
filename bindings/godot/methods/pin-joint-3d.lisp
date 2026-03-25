(common-lisp:in-package :%godot)


(defgmethod
 (pin-joint-3d+set-param :class 'pin-joint-3d :bind "set_param" :hash
  2059913726)
 :void (param pin-joint-3d+param) (value float))

(defgmethod
 (pin-joint-3d+get-param :class 'pin-joint-3d :bind "get_param" :hash
  1758438771)
 float (param pin-joint-3d+param))