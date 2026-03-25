(common-lisp:in-package :%godot)


(defgproperty texture-rect+texture 'texture-rect :get 'texture-rect+get-texture
 :set 'texture-rect+set-texture)

(defgproperty texture-rect+expand-mode 'texture-rect :get
 'texture-rect+get-expand-mode :set 'texture-rect+set-expand-mode)

(defgproperty texture-rect+stretch-mode 'texture-rect :get
 'texture-rect+get-stretch-mode :set 'texture-rect+set-stretch-mode)

(defgproperty texture-rect+flip-h 'texture-rect :get 'texture-rect+is-flipped-h
 :set 'texture-rect+set-flip-h)

(defgproperty texture-rect+flip-v 'texture-rect :get 'texture-rect+is-flipped-v
 :set 'texture-rect+set-flip-v)