(common-lisp:in-package :%godot)


(defgproperty curve+min-domain 'curve :get 'curve+get-min-domain :set
 'curve+set-min-domain)

(defgproperty curve+max-domain 'curve :get 'curve+get-max-domain :set
 'curve+set-max-domain)

(defgproperty curve+min-value 'curve :get 'curve+get-min-value :set
 'curve+set-min-value)

(defgproperty curve+max-value 'curve :get 'curve+get-max-value :set
 'curve+set-max-value)

(defgproperty curve+bake-resolution 'curve :get 'curve+get-bake-resolution :set
 'curve+set-bake-resolution)

(defgproperty curve+point-count 'curve :get 'curve+get-point-count :set
 'curve+set-point-count)