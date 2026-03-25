(common-lisp:in-package :%godot)


(defgproperty animation-node-state-machine+state-machine-type
 'animation-node-state-machine :get
 'animation-node-state-machine+get-state-machine-type :set
 'animation-node-state-machine+set-state-machine-type)

(defgproperty animation-node-state-machine+allow-transition-to-self
 'animation-node-state-machine :get
 'animation-node-state-machine+is-allow-transition-to-self :set
 'animation-node-state-machine+set-allow-transition-to-self)

(defgproperty animation-node-state-machine+reset-ends
 'animation-node-state-machine :get
 'animation-node-state-machine+are-ends-reset :set
 'animation-node-state-machine+set-reset-ends)