(common-lisp:in-package :%godot)


(defgproperty visual-shader-node-texture+source 'visual-shader-node-texture
 :get 'visual-shader-node-texture+get-source :set
 'visual-shader-node-texture+set-source)

(defgproperty visual-shader-node-texture+texture 'visual-shader-node-texture
 :get 'visual-shader-node-texture+get-texture :set
 'visual-shader-node-texture+set-texture)

(defgproperty visual-shader-node-texture+texture-type
 'visual-shader-node-texture :get 'visual-shader-node-texture+get-texture-type
 :set 'visual-shader-node-texture+set-texture-type)