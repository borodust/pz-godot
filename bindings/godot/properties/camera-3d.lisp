(common-lisp:in-package :%godot)


(defgproperty camera-3d+keep-aspect 'camera-3d :get
 'camera-3d+get-keep-aspect-mode :set 'camera-3d+set-keep-aspect-mode)

(defgproperty camera-3d+cull-mask 'camera-3d :get 'camera-3d+get-cull-mask :set
 'camera-3d+set-cull-mask)

(defgproperty camera-3d+environment 'camera-3d :get 'camera-3d+get-environment
 :set 'camera-3d+set-environment)

(defgproperty camera-3d+attributes 'camera-3d :get 'camera-3d+get-attributes
 :set 'camera-3d+set-attributes)

(defgproperty camera-3d+compositor 'camera-3d :get 'camera-3d+get-compositor
 :set 'camera-3d+set-compositor)

(defgproperty camera-3d+h-offset 'camera-3d :get 'camera-3d+get-h-offset :set
 'camera-3d+set-h-offset)

(defgproperty camera-3d+v-offset 'camera-3d :get 'camera-3d+get-v-offset :set
 'camera-3d+set-v-offset)

(defgproperty camera-3d+doppler-tracking 'camera-3d :get
 'camera-3d+get-doppler-tracking :set 'camera-3d+set-doppler-tracking)

(defgproperty camera-3d+projection 'camera-3d :get 'camera-3d+get-projection
 :set 'camera-3d+set-projection)

(defgproperty camera-3d+current 'camera-3d :get 'camera-3d+is-current :set
 'camera-3d+set-current)

(defgproperty camera-3d+fov 'camera-3d :get 'camera-3d+get-fov :set
 'camera-3d+set-fov)

(defgproperty camera-3d+size 'camera-3d :get 'camera-3d+get-size :set
 'camera-3d+set-size)

(defgproperty camera-3d+frustum-offset 'camera-3d :get
 'camera-3d+get-frustum-offset :set 'camera-3d+set-frustum-offset)

(defgproperty camera-3d+near 'camera-3d :get 'camera-3d+get-near :set
 'camera-3d+set-near)

(defgproperty camera-3d+far 'camera-3d :get 'camera-3d+get-far :set
 'camera-3d+set-far)