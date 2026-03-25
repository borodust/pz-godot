(common-lisp:in-package :%godot)


(defgproperty physics-direct-body-state-3d+step 'physics-direct-body-state-3d
 :get 'physics-direct-body-state-3d+get-step)

(defgproperty physics-direct-body-state-3d+inverse-mass
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-inverse-mass)

(defgproperty physics-direct-body-state-3d+total-angular-damp
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-total-angular-damp)

(defgproperty physics-direct-body-state-3d+total-linear-damp
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-total-linear-damp)

(defgproperty physics-direct-body-state-3d+inverse-inertia
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-inverse-inertia)

(defgproperty physics-direct-body-state-3d+inverse-inertia-tensor
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-inverse-inertia-tensor)

(defgproperty physics-direct-body-state-3d+total-gravity
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-total-gravity)

(defgproperty physics-direct-body-state-3d+center-of-mass
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-center-of-mass)

(defgproperty physics-direct-body-state-3d+center-of-mass-local
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-center-of-mass-local)

(defgproperty physics-direct-body-state-3d+principal-inertia-axes
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-principal-inertia-axes)

(defgproperty physics-direct-body-state-3d+angular-velocity
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-angular-velocity :set
 'physics-direct-body-state-3d+set-angular-velocity)

(defgproperty physics-direct-body-state-3d+linear-velocity
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-linear-velocity :set
 'physics-direct-body-state-3d+set-linear-velocity)

(defgproperty physics-direct-body-state-3d+sleeping
 'physics-direct-body-state-3d :get 'physics-direct-body-state-3d+is-sleeping
 :set 'physics-direct-body-state-3d+set-sleep-state)

(defgproperty physics-direct-body-state-3d+collision-layer
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-collision-layer :set
 'physics-direct-body-state-3d+set-collision-layer)

(defgproperty physics-direct-body-state-3d+collision-mask
 'physics-direct-body-state-3d :get
 'physics-direct-body-state-3d+get-collision-mask :set
 'physics-direct-body-state-3d+set-collision-mask)

(defgproperty physics-direct-body-state-3d+transform
 'physics-direct-body-state-3d :get 'physics-direct-body-state-3d+get-transform
 :set 'physics-direct-body-state-3d+set-transform)