(common-lisp:in-package :%godot)


(defgproperty navigation-link-2d+enabled 'navigation-link-2d :get
 'navigation-link-2d+is-enabled :set 'navigation-link-2d+set-enabled)

(defgproperty navigation-link-2d+bidirectional 'navigation-link-2d :get
 'navigation-link-2d+is-bidirectional :set
 'navigation-link-2d+set-bidirectional)

(defgproperty navigation-link-2d+navigation-layers 'navigation-link-2d :get
 'navigation-link-2d+get-navigation-layers :set
 'navigation-link-2d+set-navigation-layers)

(defgproperty navigation-link-2d+start-position 'navigation-link-2d :get
 'navigation-link-2d+get-start-position :set
 'navigation-link-2d+set-start-position)

(defgproperty navigation-link-2d+end-position 'navigation-link-2d :get
 'navigation-link-2d+get-end-position :set 'navigation-link-2d+set-end-position)

(defgproperty navigation-link-2d+enter-cost 'navigation-link-2d :get
 'navigation-link-2d+get-enter-cost :set 'navigation-link-2d+set-enter-cost)

(defgproperty navigation-link-2d+travel-cost 'navigation-link-2d :get
 'navigation-link-2d+get-travel-cost :set 'navigation-link-2d+set-travel-cost)