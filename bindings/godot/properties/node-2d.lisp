(common-lisp:in-package :%godot)


(defgproperty node-2d+position 'node-2d :get 'node-2d+get-position :set
 'node-2d+set-position)

(defgproperty node-2d+rotation 'node-2d :get 'node-2d+get-rotation :set
 'node-2d+set-rotation)

(defgproperty node-2d+rotation-degrees 'node-2d :get
 'node-2d+get-rotation-degrees :set 'node-2d+set-rotation-degrees)

(defgproperty node-2d+scale 'node-2d :get 'node-2d+get-scale :set
 'node-2d+set-scale)

(defgproperty node-2d+skew 'node-2d :get 'node-2d+get-skew :set
 'node-2d+set-skew)

(defgproperty node-2d+transform 'node-2d :set 'node-2d+set-transform)

(defgproperty node-2d+global-position 'node-2d :get
 'node-2d+get-global-position :set 'node-2d+set-global-position)

(defgproperty node-2d+global-rotation 'node-2d :get
 'node-2d+get-global-rotation :set 'node-2d+set-global-rotation)

(defgproperty node-2d+global-rotation-degrees 'node-2d :get
 'node-2d+get-global-rotation-degrees :set 'node-2d+set-global-rotation-degrees)

(defgproperty node-2d+global-scale 'node-2d :get 'node-2d+get-global-scale :set
 'node-2d+set-global-scale)

(defgproperty node-2d+global-skew 'node-2d :get 'node-2d+get-global-skew :set
 'node-2d+set-global-skew)

(defgproperty node-2d+global-transform 'node-2d :set
 'node-2d+set-global-transform)