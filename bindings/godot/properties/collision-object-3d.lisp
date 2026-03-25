(common-lisp:in-package :%godot)


(defgproperty collision-object-3d+disable-mode 'collision-object-3d :get
 'collision-object-3d+get-disable-mode :set
 'collision-object-3d+set-disable-mode)

(defgproperty collision-object-3d+collision-layer 'collision-object-3d :get
 'collision-object-3d+get-collision-layer :set
 'collision-object-3d+set-collision-layer)

(defgproperty collision-object-3d+collision-mask 'collision-object-3d :get
 'collision-object-3d+get-collision-mask :set
 'collision-object-3d+set-collision-mask)

(defgproperty collision-object-3d+collision-priority 'collision-object-3d :get
 'collision-object-3d+get-collision-priority :set
 'collision-object-3d+set-collision-priority)

(defgproperty collision-object-3d+input-ray-pickable 'collision-object-3d :get
 'collision-object-3d+is-ray-pickable :set
 'collision-object-3d+set-ray-pickable)

(defgproperty collision-object-3d+input-capture-on-drag 'collision-object-3d
 :get 'collision-object-3d+get-capture-input-on-drag :set
 'collision-object-3d+set-capture-input-on-drag)