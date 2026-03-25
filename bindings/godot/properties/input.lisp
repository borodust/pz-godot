(common-lisp:in-package :%godot)


(defgproperty input+mouse-mode 'input :get 'input+get-mouse-mode :set
 'input+set-mouse-mode)

(defgproperty input+use-accumulated-input 'input :get
 'input+is-using-accumulated-input :set 'input+set-use-accumulated-input)

(defgproperty input+emulate-mouse-from-touch 'input :get
 'input+is-emulating-mouse-from-touch :set 'input+set-emulate-mouse-from-touch)

(defgproperty input+emulate-touch-from-mouse 'input :get
 'input+is-emulating-touch-from-mouse :set 'input+set-emulate-touch-from-mouse)