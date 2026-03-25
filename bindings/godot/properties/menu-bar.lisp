(common-lisp:in-package :%godot)


(defgproperty menu-bar+flat 'menu-bar :get 'menu-bar+is-flat :set
 'menu-bar+set-flat)

(defgproperty menu-bar+start-index 'menu-bar :get 'menu-bar+get-start-index
 :set 'menu-bar+set-start-index)

(defgproperty menu-bar+switch-on-hover 'menu-bar :get
 'menu-bar+is-switch-on-hover :set 'menu-bar+set-switch-on-hover)

(defgproperty menu-bar+prefer-global-menu 'menu-bar :get
 'menu-bar+is-prefer-global-menu :set 'menu-bar+set-prefer-global-menu)

(defgproperty menu-bar+text-direction 'menu-bar :get
 'menu-bar+get-text-direction :set 'menu-bar+set-text-direction)

(defgproperty menu-bar+language 'menu-bar :get 'menu-bar+get-language :set
 'menu-bar+set-language)