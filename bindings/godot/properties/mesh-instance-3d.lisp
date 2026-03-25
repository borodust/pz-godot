(common-lisp:in-package :%godot)


(defgproperty mesh-instance-3d+mesh 'mesh-instance-3d :get
 'mesh-instance-3d+get-mesh :set 'mesh-instance-3d+set-mesh)

(defgproperty mesh-instance-3d+skin 'mesh-instance-3d :get
 'mesh-instance-3d+get-skin :set 'mesh-instance-3d+set-skin)

(defgproperty mesh-instance-3d+skeleton 'mesh-instance-3d :get
 'mesh-instance-3d+get-skeleton-path :set 'mesh-instance-3d+set-skeleton-path)