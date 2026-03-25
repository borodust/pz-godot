(common-lisp:in-package :%godot)


(defgproperty spring-arm-3d+collision-mask 'spring-arm-3d :get
 'spring-arm-3d+get-collision-mask :set 'spring-arm-3d+set-collision-mask)

(defgproperty spring-arm-3d+shape 'spring-arm-3d :get 'spring-arm-3d+get-shape
 :set 'spring-arm-3d+set-shape)

(defgproperty spring-arm-3d+spring-length 'spring-arm-3d :get
 'spring-arm-3d+get-length :set 'spring-arm-3d+set-length)

(defgproperty spring-arm-3d+margin 'spring-arm-3d :get
 'spring-arm-3d+get-margin :set 'spring-arm-3d+set-margin)