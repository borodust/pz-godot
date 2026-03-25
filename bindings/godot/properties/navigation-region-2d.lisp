(common-lisp:in-package :%godot)


(defgproperty navigation-region-2d+navigation-polygon 'navigation-region-2d
 :get 'navigation-region-2d+get-navigation-polygon :set
 'navigation-region-2d+set-navigation-polygon)

(defgproperty navigation-region-2d+enabled 'navigation-region-2d :get
 'navigation-region-2d+is-enabled :set 'navigation-region-2d+set-enabled)

(defgproperty navigation-region-2d+use-edge-connections 'navigation-region-2d
 :get 'navigation-region-2d+get-use-edge-connections :set
 'navigation-region-2d+set-use-edge-connections)

(defgproperty navigation-region-2d+navigation-layers 'navigation-region-2d :get
 'navigation-region-2d+get-navigation-layers :set
 'navigation-region-2d+set-navigation-layers)

(defgproperty navigation-region-2d+enter-cost 'navigation-region-2d :get
 'navigation-region-2d+get-enter-cost :set 'navigation-region-2d+set-enter-cost)

(defgproperty navigation-region-2d+travel-cost 'navigation-region-2d :get
 'navigation-region-2d+get-travel-cost :set
 'navigation-region-2d+set-travel-cost)