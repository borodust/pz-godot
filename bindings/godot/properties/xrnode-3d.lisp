(common-lisp:in-package :%godot)


(defgproperty xrnode-3d+tracker 'xrnode-3d :get 'xrnode-3d+get-tracker :set
 'xrnode-3d+set-tracker)

(defgproperty xrnode-3d+pose 'xrnode-3d :get 'xrnode-3d+get-pose-name :set
 'xrnode-3d+set-pose-name)

(defgproperty xrnode-3d+show-when-tracked 'xrnode-3d :get
 'xrnode-3d+get-show-when-tracked :set 'xrnode-3d+set-show-when-tracked)