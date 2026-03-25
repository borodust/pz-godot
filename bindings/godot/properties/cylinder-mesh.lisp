(common-lisp:in-package :%godot)


(defgproperty cylinder-mesh+top-radius 'cylinder-mesh :get
 'cylinder-mesh+get-top-radius :set 'cylinder-mesh+set-top-radius)

(defgproperty cylinder-mesh+bottom-radius 'cylinder-mesh :get
 'cylinder-mesh+get-bottom-radius :set 'cylinder-mesh+set-bottom-radius)

(defgproperty cylinder-mesh+height 'cylinder-mesh :get
 'cylinder-mesh+get-height :set 'cylinder-mesh+set-height)

(defgproperty cylinder-mesh+radial-segments 'cylinder-mesh :get
 'cylinder-mesh+get-radial-segments :set 'cylinder-mesh+set-radial-segments)

(defgproperty cylinder-mesh+rings 'cylinder-mesh :get 'cylinder-mesh+get-rings
 :set 'cylinder-mesh+set-rings)

(defgproperty cylinder-mesh+cap-top 'cylinder-mesh :get
 'cylinder-mesh+is-cap-top :set 'cylinder-mesh+set-cap-top)

(defgproperty cylinder-mesh+cap-bottom 'cylinder-mesh :get
 'cylinder-mesh+is-cap-bottom :set 'cylinder-mesh+set-cap-bottom)