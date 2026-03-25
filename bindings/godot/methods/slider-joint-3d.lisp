(common-lisp:in-package :%godot)


(defgmethod
 (slider-joint-3d+set-param :class 'slider-joint-3d :bind "set_param" :hash
  918243683)
 :void (param slider-joint-3d+param) (value float))

(defgmethod
 (slider-joint-3d+get-param :class 'slider-joint-3d :bind "get_param" :hash
  959925627)
 float (param slider-joint-3d+param))