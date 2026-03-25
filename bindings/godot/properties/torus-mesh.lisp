(common-lisp:in-package :%godot)


(defgproperty torus-mesh+inner-radius 'torus-mesh :get
 'torus-mesh+get-inner-radius :set 'torus-mesh+set-inner-radius)

(defgproperty torus-mesh+outer-radius 'torus-mesh :get
 'torus-mesh+get-outer-radius :set 'torus-mesh+set-outer-radius)

(defgproperty torus-mesh+rings 'torus-mesh :get 'torus-mesh+get-rings :set
 'torus-mesh+set-rings)

(defgproperty torus-mesh+ring-segments 'torus-mesh :get
 'torus-mesh+get-ring-segments :set 'torus-mesh+set-ring-segments)