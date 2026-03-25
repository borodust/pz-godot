(common-lisp:in-package :%godot)


(defgproperty placeholder-texture-layered+size 'placeholder-texture-layered
 :get 'placeholder-texture-layered+get-size :set
 'placeholder-texture-layered+set-size)

(defgproperty placeholder-texture-layered+layers 'placeholder-texture-layered
 :set 'placeholder-texture-layered+set-layers)