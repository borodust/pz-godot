(common-lisp:in-package :%godot)


(defgproperty animation+length 'animation :get 'animation+get-length :set
 'animation+set-length)

(defgproperty animation+loop-mode 'animation :get 'animation+get-loop-mode :set
 'animation+set-loop-mode)

(defgproperty animation+step 'animation :get 'animation+get-step :set
 'animation+set-step)

(defgproperty animation+capture-included 'animation :get
 'animation+is-capture-included)