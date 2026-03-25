(common-lisp:in-package :%godot)


(defgproperty mesh-texture+mesh 'mesh-texture :get 'mesh-texture+get-mesh :set
 'mesh-texture+set-mesh)

(defgproperty mesh-texture+base-texture 'mesh-texture :get
 'mesh-texture+get-base-texture :set 'mesh-texture+set-base-texture)

(defgproperty mesh-texture+image-size 'mesh-texture :get
 'mesh-texture+get-image-size :set 'mesh-texture+set-image-size)