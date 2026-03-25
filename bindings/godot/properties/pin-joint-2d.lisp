(common-lisp:in-package :%godot)


(defgproperty pin-joint-2d+softness 'pin-joint-2d :get
 'pin-joint-2d+get-softness :set 'pin-joint-2d+set-softness)

(defgproperty pin-joint-2d+angular-limit-enabled 'pin-joint-2d :get
 'pin-joint-2d+is-angular-limit-enabled :set
 'pin-joint-2d+set-angular-limit-enabled)

(defgproperty pin-joint-2d+angular-limit-lower 'pin-joint-2d :get
 'pin-joint-2d+get-angular-limit-lower :set
 'pin-joint-2d+set-angular-limit-lower)

(defgproperty pin-joint-2d+angular-limit-upper 'pin-joint-2d :get
 'pin-joint-2d+get-angular-limit-upper :set
 'pin-joint-2d+set-angular-limit-upper)

(defgproperty pin-joint-2d+motor-enabled 'pin-joint-2d :get
 'pin-joint-2d+is-motor-enabled :set 'pin-joint-2d+set-motor-enabled)

(defgproperty pin-joint-2d+motor-target-velocity 'pin-joint-2d :get
 'pin-joint-2d+get-motor-target-velocity :set
 'pin-joint-2d+set-motor-target-velocity)