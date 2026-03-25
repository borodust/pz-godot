(common-lisp:in-package :%godot)


(defgproperty navigation-region-3d+navigation-mesh 'navigation-region-3d :get
 'navigation-region-3d+get-navigation-mesh :set
 'navigation-region-3d+set-navigation-mesh)

(defgproperty navigation-region-3d+enabled 'navigation-region-3d :get
 'navigation-region-3d+is-enabled :set 'navigation-region-3d+set-enabled)

(defgproperty navigation-region-3d+use-edge-connections 'navigation-region-3d
 :get 'navigation-region-3d+get-use-edge-connections :set
 'navigation-region-3d+set-use-edge-connections)

(defgproperty navigation-region-3d+navigation-layers 'navigation-region-3d :get
 'navigation-region-3d+get-navigation-layers :set
 'navigation-region-3d+set-navigation-layers)

(defgproperty navigation-region-3d+enter-cost 'navigation-region-3d :get
 'navigation-region-3d+get-enter-cost :set 'navigation-region-3d+set-enter-cost)

(defgproperty navigation-region-3d+travel-cost 'navigation-region-3d :get
 'navigation-region-3d+get-travel-cost :set
 'navigation-region-3d+set-travel-cost)