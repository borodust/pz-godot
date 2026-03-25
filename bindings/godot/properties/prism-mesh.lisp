(common-lisp:in-package :%godot)


(defgproperty prism-mesh+left-to-right 'prism-mesh :get
 'prism-mesh+get-left-to-right :set 'prism-mesh+set-left-to-right)

(defgproperty prism-mesh+size 'prism-mesh :get 'prism-mesh+get-size :set
 'prism-mesh+set-size)

(defgproperty prism-mesh+subdivide-width 'prism-mesh :get
 'prism-mesh+get-subdivide-width :set 'prism-mesh+set-subdivide-width)

(defgproperty prism-mesh+subdivide-height 'prism-mesh :get
 'prism-mesh+get-subdivide-height :set 'prism-mesh+set-subdivide-height)

(defgproperty prism-mesh+subdivide-depth 'prism-mesh :get
 'prism-mesh+get-subdivide-depth :set 'prism-mesh+set-subdivide-depth)