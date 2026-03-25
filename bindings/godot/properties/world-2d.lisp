(common-lisp:in-package :%godot)


(defgproperty world-2d+canvas 'world-2d :get 'world-2d+get-canvas)

(defgproperty world-2d+navigation-map 'world-2d :get
 'world-2d+get-navigation-map)

(defgproperty world-2d+space 'world-2d :get 'world-2d+get-space)

(defgproperty world-2d+direct-space-state 'world-2d :get
 'world-2d+get-direct-space-state)