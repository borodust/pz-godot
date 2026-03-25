(common-lisp:in-package :%godot)


(defgmethod
 (joint-limitation-cone-3d+set-angle :class 'joint-limitation-cone-3d :bind
  "set_angle" :hash 373806689)
 :void (angle float))

(defgmethod
 (joint-limitation-cone-3d+get-angle :class 'joint-limitation-cone-3d :bind
  "get_angle" :hash 1740695150)
 float)