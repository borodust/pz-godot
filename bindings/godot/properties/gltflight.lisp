(common-lisp:in-package :%godot)


(defgproperty gltflight+color 'gltflight :get 'gltflight+get-color :set
 'gltflight+set-color)

(defgproperty gltflight+intensity 'gltflight :get 'gltflight+get-intensity :set
 'gltflight+set-intensity)

(defgproperty gltflight+light-type 'gltflight :get 'gltflight+get-light-type
 :set 'gltflight+set-light-type)

(defgproperty gltflight+range 'gltflight :get 'gltflight+get-range :set
 'gltflight+set-range)

(defgproperty gltflight+inner-cone-angle 'gltflight :get
 'gltflight+get-inner-cone-angle :set 'gltflight+set-inner-cone-angle)

(defgproperty gltflight+outer-cone-angle 'gltflight :get
 'gltflight+get-outer-cone-angle :set 'gltflight+set-outer-cone-angle)