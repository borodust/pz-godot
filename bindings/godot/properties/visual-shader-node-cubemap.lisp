(common-lisp:in-package :%godot)


(defgproperty visual-shader-node-cubemap+source 'visual-shader-node-cubemap
 :get 'visual-shader-node-cubemap+get-source :set
 'visual-shader-node-cubemap+set-source)

(defgproperty visual-shader-node-cubemap+cube-map 'visual-shader-node-cubemap
 :get 'visual-shader-node-cubemap+get-cube-map :set
 'visual-shader-node-cubemap+set-cube-map)

(defgproperty visual-shader-node-cubemap+texture-type
 'visual-shader-node-cubemap :get 'visual-shader-node-cubemap+get-texture-type
 :set 'visual-shader-node-cubemap+set-texture-type)