(common-lisp:in-package :%godot)


(defgproperty gradient-texture-1d+gradient 'gradient-texture-1d :get
 'gradient-texture-1d+get-gradient :set 'gradient-texture-1d+set-gradient)

(defgproperty gradient-texture-1d+width 'gradient-texture-1d :set
 'gradient-texture-1d+set-width)

(defgproperty gradient-texture-1d+use-hdr 'gradient-texture-1d :get
 'gradient-texture-1d+is-using-hdr :set 'gradient-texture-1d+set-use-hdr)