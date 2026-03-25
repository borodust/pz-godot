(common-lisp:in-package :%godot)


(defgproperty canvas-group+fit-margin 'canvas-group :get
 'canvas-group+get-fit-margin :set 'canvas-group+set-fit-margin)

(defgproperty canvas-group+clear-margin 'canvas-group :get
 'canvas-group+get-clear-margin :set 'canvas-group+set-clear-margin)

(defgproperty canvas-group+use-mipmaps 'canvas-group :get
 'canvas-group+is-using-mipmaps :set 'canvas-group+set-use-mipmaps)