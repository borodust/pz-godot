(common-lisp:in-package :%godot)


(defgproperty world-3d+environment 'world-3d :get 'world-3d+get-environment
 :set 'world-3d+set-environment)

(defgproperty world-3d+fallback-environment 'world-3d :get
 'world-3d+get-fallback-environment :set 'world-3d+set-fallback-environment)

(defgproperty world-3d+camera-attributes 'world-3d :get
 'world-3d+get-camera-attributes :set 'world-3d+set-camera-attributes)

(defgproperty world-3d+space 'world-3d :get 'world-3d+get-space)

(defgproperty world-3d+navigation-map 'world-3d :get
 'world-3d+get-navigation-map)

(defgproperty world-3d+scenario 'world-3d :get 'world-3d+get-scenario)

(defgproperty world-3d+direct-space-state 'world-3d :get
 'world-3d+get-direct-space-state)