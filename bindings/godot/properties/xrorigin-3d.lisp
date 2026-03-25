(common-lisp:in-package :%godot)


(defgproperty xrorigin-3d+world-scale 'xrorigin-3d :get
 'xrorigin-3d+get-world-scale :set 'xrorigin-3d+set-world-scale)

(defgproperty xrorigin-3d+current 'xrorigin-3d :get 'xrorigin-3d+is-current
 :set 'xrorigin-3d+set-current)