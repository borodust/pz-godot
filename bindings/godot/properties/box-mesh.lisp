(common-lisp:in-package :%godot)


(defgproperty box-mesh+size 'box-mesh :get 'box-mesh+get-size :set
 'box-mesh+set-size)

(defgproperty box-mesh+subdivide-width 'box-mesh :get
 'box-mesh+get-subdivide-width :set 'box-mesh+set-subdivide-width)

(defgproperty box-mesh+subdivide-height 'box-mesh :get
 'box-mesh+get-subdivide-height :set 'box-mesh+set-subdivide-height)

(defgproperty box-mesh+subdivide-depth 'box-mesh :get
 'box-mesh+get-subdivide-depth :set 'box-mesh+set-subdivide-depth)