(common-lisp:in-package :%godot)


(defgproperty flow-container+alignment 'flow-container :get
 'flow-container+get-alignment :set 'flow-container+set-alignment)

(defgproperty flow-container+last-wrap-alignment 'flow-container :get
 'flow-container+get-last-wrap-alignment :set
 'flow-container+set-last-wrap-alignment)

(defgproperty flow-container+vertical 'flow-container :get
 'flow-container+is-vertical :set 'flow-container+set-vertical)

(defgproperty flow-container+reverse-fill 'flow-container :get
 'flow-container+is-reverse-fill :set 'flow-container+set-reverse-fill)