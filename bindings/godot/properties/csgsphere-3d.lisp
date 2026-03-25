(common-lisp:in-package :%godot)


(defgproperty csgsphere-3d+radius 'csgsphere-3d :get 'csgsphere-3d+get-radius
 :set 'csgsphere-3d+set-radius)

(defgproperty csgsphere-3d+radial-segments 'csgsphere-3d :get
 'csgsphere-3d+get-radial-segments :set 'csgsphere-3d+set-radial-segments)

(defgproperty csgsphere-3d+rings 'csgsphere-3d :get 'csgsphere-3d+get-rings
 :set 'csgsphere-3d+set-rings)

(defgproperty csgsphere-3d+smooth-faces 'csgsphere-3d :get
 'csgsphere-3d+get-smooth-faces :set 'csgsphere-3d+set-smooth-faces)

(defgproperty csgsphere-3d+material 'csgsphere-3d :get
 'csgsphere-3d+get-material :set 'csgsphere-3d+set-material)