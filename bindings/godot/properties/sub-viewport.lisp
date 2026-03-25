(common-lisp:in-package :%godot)


(defgproperty sub-viewport+size 'sub-viewport :get 'sub-viewport+get-size :set
 'sub-viewport+set-size)

(defgproperty sub-viewport+size-2d-override 'sub-viewport :get
 'sub-viewport+get-size-2d-override :set 'sub-viewport+set-size-2d-override)

(defgproperty sub-viewport+size-2d-override-stretch 'sub-viewport :get
 'sub-viewport+is-size-2d-override-stretch-enabled :set
 'sub-viewport+set-size-2d-override-stretch)

(defgproperty sub-viewport+render-target-clear-mode 'sub-viewport :get
 'sub-viewport+get-clear-mode :set 'sub-viewport+set-clear-mode)

(defgproperty sub-viewport+render-target-update-mode 'sub-viewport :get
 'sub-viewport+get-update-mode :set 'sub-viewport+set-update-mode)