(common-lisp:in-package :%godot)


(defgproperty material+render-priority 'material :get
 'material+get-render-priority :set 'material+set-render-priority)

(defgproperty material+next-pass 'material :get 'material+get-next-pass :set
 'material+set-next-pass)