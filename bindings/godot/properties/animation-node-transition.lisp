(common-lisp:in-package :%godot)


(defgproperty animation-node-transition+xfade-time 'animation-node-transition
 :get 'animation-node-transition+get-xfade-time :set
 'animation-node-transition+set-xfade-time)

(defgproperty animation-node-transition+xfade-curve 'animation-node-transition
 :get 'animation-node-transition+get-xfade-curve :set
 'animation-node-transition+set-xfade-curve)

(defgproperty animation-node-transition+allow-transition-to-self
 'animation-node-transition :get
 'animation-node-transition+is-allow-transition-to-self :set
 'animation-node-transition+set-allow-transition-to-self)

(defgproperty animation-node-transition+input-count 'animation-node-transition
 :set 'animation-node-transition+set-input-count)