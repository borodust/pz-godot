(common-lisp:in-package :%godot)


(defgproperty occluder-instance-3d+occluder 'occluder-instance-3d :get
 'occluder-instance-3d+get-occluder :set 'occluder-instance-3d+set-occluder)

(defgproperty occluder-instance-3d+bake-mask 'occluder-instance-3d :get
 'occluder-instance-3d+get-bake-mask :set 'occluder-instance-3d+set-bake-mask)

(defgproperty occluder-instance-3d+bake-simplification-distance
 'occluder-instance-3d :get
 'occluder-instance-3d+get-bake-simplification-distance :set
 'occluder-instance-3d+set-bake-simplification-distance)