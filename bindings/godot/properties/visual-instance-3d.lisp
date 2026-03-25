(common-lisp:in-package :%godot)


(defgproperty visual-instance-3d+layers 'visual-instance-3d :get
 'visual-instance-3d+get-layer-mask :set 'visual-instance-3d+set-layer-mask)

(defgproperty visual-instance-3d+sorting-offset 'visual-instance-3d :get
 'visual-instance-3d+get-sorting-offset :set
 'visual-instance-3d+set-sorting-offset)

(defgproperty visual-instance-3d+sorting-use-aabb-center 'visual-instance-3d
 :get 'visual-instance-3d+is-sorting-use-aabb-center :set
 'visual-instance-3d+set-sorting-use-aabb-center)