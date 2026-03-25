(common-lisp:in-package :%godot)


(defgproperty sub-viewport-container+stretch 'sub-viewport-container :get
 'sub-viewport-container+is-stretch-enabled :set
 'sub-viewport-container+set-stretch)

(defgproperty sub-viewport-container+stretch-shrink 'sub-viewport-container
 :get 'sub-viewport-container+get-stretch-shrink :set
 'sub-viewport-container+set-stretch-shrink)

(defgproperty sub-viewport-container+mouse-target 'sub-viewport-container :get
 'sub-viewport-container+is-mouse-target-enabled :set
 'sub-viewport-container+set-mouse-target)