(common-lisp:in-package :%godot)


(defgproperty plane-mesh+size 'plane-mesh :get 'plane-mesh+get-size :set
 'plane-mesh+set-size)

(defgproperty plane-mesh+subdivide-width 'plane-mesh :get
 'plane-mesh+get-subdivide-width :set 'plane-mesh+set-subdivide-width)

(defgproperty plane-mesh+subdivide-depth 'plane-mesh :get
 'plane-mesh+get-subdivide-depth :set 'plane-mesh+set-subdivide-depth)

(defgproperty plane-mesh+center-offset 'plane-mesh :get
 'plane-mesh+get-center-offset :set 'plane-mesh+set-center-offset)

(defgproperty plane-mesh+orientation 'plane-mesh :get
 'plane-mesh+get-orientation :set 'plane-mesh+set-orientation)