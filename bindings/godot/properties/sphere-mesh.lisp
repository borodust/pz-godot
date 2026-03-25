(common-lisp:in-package :%godot)


(defgproperty sphere-mesh+radius 'sphere-mesh :get 'sphere-mesh+get-radius :set
 'sphere-mesh+set-radius)

(defgproperty sphere-mesh+height 'sphere-mesh :get 'sphere-mesh+get-height :set
 'sphere-mesh+set-height)

(defgproperty sphere-mesh+radial-segments 'sphere-mesh :get
 'sphere-mesh+get-radial-segments :set 'sphere-mesh+set-radial-segments)

(defgproperty sphere-mesh+rings 'sphere-mesh :get 'sphere-mesh+get-rings :set
 'sphere-mesh+set-rings)

(defgproperty sphere-mesh+is-hemisphere 'sphere-mesh :get
 'sphere-mesh+get-is-hemisphere :set 'sphere-mesh+set-is-hemisphere)