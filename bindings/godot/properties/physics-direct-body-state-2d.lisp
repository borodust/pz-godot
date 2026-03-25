(common-lisp:in-package :%godot)


(defgproperty physics-direct-body-state-2d+step 'physics-direct-body-state-2d
 :get 'physics-direct-body-state-2d+get-step)

(defgproperty physics-direct-body-state-2d+inverse-mass
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-inverse-mass)

(defgproperty physics-direct-body-state-2d+inverse-inertia
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-inverse-inertia)

(defgproperty physics-direct-body-state-2d+total-angular-damp
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-total-angular-damp)

(defgproperty physics-direct-body-state-2d+total-linear-damp
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-total-linear-damp)

(defgproperty physics-direct-body-state-2d+total-gravity
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-total-gravity)

(defgproperty physics-direct-body-state-2d+center-of-mass
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-center-of-mass)

(defgproperty physics-direct-body-state-2d+center-of-mass-local
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-center-of-mass-local)

(defgproperty physics-direct-body-state-2d+angular-velocity
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-angular-velocity :set
 'physics-direct-body-state-2d+set-angular-velocity)

(defgproperty physics-direct-body-state-2d+linear-velocity
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-linear-velocity :set
 'physics-direct-body-state-2d+set-linear-velocity)

(defgproperty physics-direct-body-state-2d+sleeping
 'physics-direct-body-state-2d :get 'physics-direct-body-state-2d+is-sleeping
 :set 'physics-direct-body-state-2d+set-sleep-state)

(defgproperty physics-direct-body-state-2d+collision-layer
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-collision-layer :set
 'physics-direct-body-state-2d+set-collision-layer)

(defgproperty physics-direct-body-state-2d+collision-mask
 'physics-direct-body-state-2d :get
 'physics-direct-body-state-2d+get-collision-mask :set
 'physics-direct-body-state-2d+set-collision-mask)

(defgproperty physics-direct-body-state-2d+transform
 'physics-direct-body-state-2d :get 'physics-direct-body-state-2d+get-transform
 :set 'physics-direct-body-state-2d+set-transform)