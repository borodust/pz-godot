(common-lisp:in-package :%godot)


(defgproperty skeleton-3d+motion-scale 'skeleton-3d :get
 'skeleton-3d+get-motion-scale :set 'skeleton-3d+set-motion-scale)

(defgproperty skeleton-3d+show-rest-only 'skeleton-3d :get
 'skeleton-3d+is-show-rest-only :set 'skeleton-3d+set-show-rest-only)

(defgproperty skeleton-3d+modifier-callback-mode-process 'skeleton-3d :get
 'skeleton-3d+get-modifier-callback-mode-process :set
 'skeleton-3d+set-modifier-callback-mode-process)

(defgproperty skeleton-3d+animate-physical-bones 'skeleton-3d :get
 'skeleton-3d+get-animate-physical-bones :set
 'skeleton-3d+set-animate-physical-bones)