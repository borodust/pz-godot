(common-lisp:in-package :%godot)


(defgproperty soft-body-3d+collision-layer 'soft-body-3d :get
 'soft-body-3d+get-collision-layer :set 'soft-body-3d+set-collision-layer)

(defgproperty soft-body-3d+collision-mask 'soft-body-3d :get
 'soft-body-3d+get-collision-mask :set 'soft-body-3d+set-collision-mask)

(defgproperty soft-body-3d+parent-collision-ignore 'soft-body-3d :get
 'soft-body-3d+get-parent-collision-ignore :set
 'soft-body-3d+set-parent-collision-ignore)

(defgproperty soft-body-3d+simulation-precision 'soft-body-3d :get
 'soft-body-3d+get-simulation-precision :set
 'soft-body-3d+set-simulation-precision)

(defgproperty soft-body-3d+total-mass 'soft-body-3d :get
 'soft-body-3d+get-total-mass :set 'soft-body-3d+set-total-mass)

(defgproperty soft-body-3d+linear-stiffness 'soft-body-3d :get
 'soft-body-3d+get-linear-stiffness :set 'soft-body-3d+set-linear-stiffness)

(defgproperty soft-body-3d+shrinking-factor 'soft-body-3d :get
 'soft-body-3d+get-shrinking-factor :set 'soft-body-3d+set-shrinking-factor)

(defgproperty soft-body-3d+pressure-coefficient 'soft-body-3d :get
 'soft-body-3d+get-pressure-coefficient :set
 'soft-body-3d+set-pressure-coefficient)

(defgproperty soft-body-3d+damping-coefficient 'soft-body-3d :get
 'soft-body-3d+get-damping-coefficient :set
 'soft-body-3d+set-damping-coefficient)

(defgproperty soft-body-3d+drag-coefficient 'soft-body-3d :get
 'soft-body-3d+get-drag-coefficient :set 'soft-body-3d+set-drag-coefficient)

(defgproperty soft-body-3d+ray-pickable 'soft-body-3d :get
 'soft-body-3d+is-ray-pickable :set 'soft-body-3d+set-ray-pickable)

(defgproperty soft-body-3d+disable-mode 'soft-body-3d :get
 'soft-body-3d+get-disable-mode :set 'soft-body-3d+set-disable-mode)