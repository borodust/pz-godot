(common-lisp:in-package :%godot)


(defgmethod
 (hinge-joint-3d+set-param :class 'hinge-joint-3d :bind "set_param" :hash
  3082977519)
 :void (param hinge-joint-3d+param) (value float))

(defgmethod
 (hinge-joint-3d+get-param :class 'hinge-joint-3d :bind "get_param" :hash
  4066002676)
 float (param hinge-joint-3d+param))

(defgmethod
 (hinge-joint-3d+set-flag :class 'hinge-joint-3d :bind "set_flag" :hash
  1083494620)
 :void (flag hinge-joint-3d+flag) (enabled bool))

(defgmethod
 (hinge-joint-3d+get-flag :class 'hinge-joint-3d :bind "get_flag" :hash
  2841369610)
 bool (flag hinge-joint-3d+flag))