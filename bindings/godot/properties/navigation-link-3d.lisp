(common-lisp:in-package :%godot)


(defgproperty navigation-link-3d+enabled 'navigation-link-3d :get
 'navigation-link-3d+is-enabled :set 'navigation-link-3d+set-enabled)

(defgproperty navigation-link-3d+bidirectional 'navigation-link-3d :get
 'navigation-link-3d+is-bidirectional :set
 'navigation-link-3d+set-bidirectional)

(defgproperty navigation-link-3d+navigation-layers 'navigation-link-3d :get
 'navigation-link-3d+get-navigation-layers :set
 'navigation-link-3d+set-navigation-layers)

(defgproperty navigation-link-3d+start-position 'navigation-link-3d :get
 'navigation-link-3d+get-start-position :set
 'navigation-link-3d+set-start-position)

(defgproperty navigation-link-3d+end-position 'navigation-link-3d :get
 'navigation-link-3d+get-end-position :set 'navigation-link-3d+set-end-position)

(defgproperty navigation-link-3d+enter-cost 'navigation-link-3d :get
 'navigation-link-3d+get-enter-cost :set 'navigation-link-3d+set-enter-cost)

(defgproperty navigation-link-3d+travel-cost 'navigation-link-3d :get
 'navigation-link-3d+get-travel-cost :set 'navigation-link-3d+set-travel-cost)