(common-lisp:in-package :%godot)


(defgproperty spring-bone-collision-3d+bone-name 'spring-bone-collision-3d :get
 'spring-bone-collision-3d+get-bone-name :set
 'spring-bone-collision-3d+set-bone-name)

(defgproperty spring-bone-collision-3d+bone 'spring-bone-collision-3d :get
 'spring-bone-collision-3d+get-bone :set 'spring-bone-collision-3d+set-bone)

(defgproperty spring-bone-collision-3d+position-offset
 'spring-bone-collision-3d :get 'spring-bone-collision-3d+get-position-offset
 :set 'spring-bone-collision-3d+set-position-offset)

(defgproperty spring-bone-collision-3d+rotation-offset
 'spring-bone-collision-3d :get 'spring-bone-collision-3d+get-rotation-offset
 :set 'spring-bone-collision-3d+set-rotation-offset)