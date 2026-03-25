(common-lisp:in-package :%godot)


(defgproperty collision-object-2d+disable-mode 'collision-object-2d :get
 'collision-object-2d+get-disable-mode :set
 'collision-object-2d+set-disable-mode)

(defgproperty collision-object-2d+collision-layer 'collision-object-2d :get
 'collision-object-2d+get-collision-layer :set
 'collision-object-2d+set-collision-layer)

(defgproperty collision-object-2d+collision-mask 'collision-object-2d :get
 'collision-object-2d+get-collision-mask :set
 'collision-object-2d+set-collision-mask)

(defgproperty collision-object-2d+collision-priority 'collision-object-2d :get
 'collision-object-2d+get-collision-priority :set
 'collision-object-2d+set-collision-priority)

(defgproperty collision-object-2d+input-pickable 'collision-object-2d :get
 'collision-object-2d+is-pickable :set 'collision-object-2d+set-pickable)