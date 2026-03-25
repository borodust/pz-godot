(common-lisp:in-package :%godot)


(defgproperty text-line+direction 'text-line :get 'text-line+get-direction :set
 'text-line+set-direction)

(defgproperty text-line+orientation 'text-line :get 'text-line+get-orientation
 :set 'text-line+set-orientation)

(defgproperty text-line+preserve-invalid 'text-line :get
 'text-line+get-preserve-invalid :set 'text-line+set-preserve-invalid)

(defgproperty text-line+preserve-control 'text-line :get
 'text-line+get-preserve-control :set 'text-line+set-preserve-control)

(defgproperty text-line+width 'text-line :get 'text-line+get-width :set
 'text-line+set-width)

(defgproperty text-line+alignment 'text-line :get
 'text-line+get-horizontal-alignment :set 'text-line+set-horizontal-alignment)

(defgproperty text-line+flags 'text-line :get 'text-line+get-flags :set
 'text-line+set-flags)

(defgproperty text-line+text-overrun-behavior 'text-line :get
 'text-line+get-text-overrun-behavior :set 'text-line+set-text-overrun-behavior)

(defgproperty text-line+ellipsis-char 'text-line :get
 'text-line+get-ellipsis-char :set 'text-line+set-ellipsis-char)