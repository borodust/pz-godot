(common-lisp:in-package :%godot)


(defgproperty gradient+interpolation-mode 'gradient :get
 'gradient+get-interpolation-mode :set 'gradient+set-interpolation-mode)

(defgproperty gradient+interpolation-color-space 'gradient :get
 'gradient+get-interpolation-color-space :set
 'gradient+set-interpolation-color-space)

(defgproperty gradient+offsets 'gradient :get 'gradient+get-offsets :set
 'gradient+set-offsets)

(defgproperty gradient+colors 'gradient :get 'gradient+get-colors :set
 'gradient+set-colors)