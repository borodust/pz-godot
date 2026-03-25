(common-lisp:in-package :%godot)


(defgproperty gradient-texture-2d+gradient 'gradient-texture-2d :get
 'gradient-texture-2d+get-gradient :set 'gradient-texture-2d+set-gradient)

(defgproperty gradient-texture-2d+width 'gradient-texture-2d :set
 'gradient-texture-2d+set-width)

(defgproperty gradient-texture-2d+height 'gradient-texture-2d :set
 'gradient-texture-2d+set-height)

(defgproperty gradient-texture-2d+use-hdr 'gradient-texture-2d :get
 'gradient-texture-2d+is-using-hdr :set 'gradient-texture-2d+set-use-hdr)

(defgproperty gradient-texture-2d+fill 'gradient-texture-2d :get
 'gradient-texture-2d+get-fill :set 'gradient-texture-2d+set-fill)

(defgproperty gradient-texture-2d+fill-from 'gradient-texture-2d :get
 'gradient-texture-2d+get-fill-from :set 'gradient-texture-2d+set-fill-from)

(defgproperty gradient-texture-2d+fill-to 'gradient-texture-2d :get
 'gradient-texture-2d+get-fill-to :set 'gradient-texture-2d+set-fill-to)

(defgproperty gradient-texture-2d+repeat 'gradient-texture-2d :get
 'gradient-texture-2d+get-repeat :set 'gradient-texture-2d+set-repeat)