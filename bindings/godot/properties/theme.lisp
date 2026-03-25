(common-lisp:in-package :%godot)


(defgproperty theme+default-base-scale 'theme :get
 'theme+get-default-base-scale :set 'theme+set-default-base-scale)

(defgproperty theme+default-font 'theme :get 'theme+get-default-font :set
 'theme+set-default-font)

(defgproperty theme+default-font-size 'theme :get 'theme+get-default-font-size
 :set 'theme+set-default-font-size)