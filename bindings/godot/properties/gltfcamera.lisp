(common-lisp:in-package :%godot)


(defgproperty gltfcamera+perspective 'gltfcamera :get
 'gltfcamera+get-perspective :set 'gltfcamera+set-perspective)

(defgproperty gltfcamera+fov 'gltfcamera :get 'gltfcamera+get-fov :set
 'gltfcamera+set-fov)

(defgproperty gltfcamera+size-mag 'gltfcamera :get 'gltfcamera+get-size-mag
 :set 'gltfcamera+set-size-mag)

(defgproperty gltfcamera+depth-far 'gltfcamera :get 'gltfcamera+get-depth-far
 :set 'gltfcamera+set-depth-far)

(defgproperty gltfcamera+depth-near 'gltfcamera :get 'gltfcamera+get-depth-near
 :set 'gltfcamera+set-depth-near)