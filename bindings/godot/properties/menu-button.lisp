(common-lisp:in-package :%godot)


(defgproperty menu-button+switch-on-hover 'menu-button :get
 'menu-button+is-switch-on-hover :set 'menu-button+set-switch-on-hover)

(defgproperty menu-button+item-count 'menu-button :get
 'menu-button+get-item-count :set 'menu-button+set-item-count)