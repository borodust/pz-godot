(common-lisp:in-package :%godot)


(defgproperty capsule-mesh+radius 'capsule-mesh :get 'capsule-mesh+get-radius
 :set 'capsule-mesh+set-radius)

(defgproperty capsule-mesh+height 'capsule-mesh :get 'capsule-mesh+get-height
 :set 'capsule-mesh+set-height)

(defgproperty capsule-mesh+radial-segments 'capsule-mesh :get
 'capsule-mesh+get-radial-segments :set 'capsule-mesh+set-radial-segments)

(defgproperty capsule-mesh+rings 'capsule-mesh :get 'capsule-mesh+get-rings
 :set 'capsule-mesh+set-rings)