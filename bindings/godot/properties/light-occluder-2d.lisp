(common-lisp:in-package :%godot)


(defgproperty light-occluder-2d+occluder 'light-occluder-2d :get
 'light-occluder-2d+get-occluder-polygon :set
 'light-occluder-2d+set-occluder-polygon)

(defgproperty light-occluder-2d+sdf-collision 'light-occluder-2d :get
 'light-occluder-2d+is-set-as-sdf-collision :set
 'light-occluder-2d+set-as-sdf-collision)

(defgproperty light-occluder-2d+occluder-light-mask 'light-occluder-2d :get
 'light-occluder-2d+get-occluder-light-mask :set
 'light-occluder-2d+set-occluder-light-mask)