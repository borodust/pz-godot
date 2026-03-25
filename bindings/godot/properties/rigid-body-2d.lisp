(common-lisp:in-package :%godot)


(defgproperty rigid-body-2d+mass 'rigid-body-2d :get 'rigid-body-2d+get-mass
 :set 'rigid-body-2d+set-mass)

(defgproperty rigid-body-2d+physics-material-override 'rigid-body-2d :get
 'rigid-body-2d+get-physics-material-override :set
 'rigid-body-2d+set-physics-material-override)

(defgproperty rigid-body-2d+gravity-scale 'rigid-body-2d :get
 'rigid-body-2d+get-gravity-scale :set 'rigid-body-2d+set-gravity-scale)

(defgproperty rigid-body-2d+center-of-mass-mode 'rigid-body-2d :get
 'rigid-body-2d+get-center-of-mass-mode :set
 'rigid-body-2d+set-center-of-mass-mode)

(defgproperty rigid-body-2d+center-of-mass 'rigid-body-2d :get
 'rigid-body-2d+get-center-of-mass :set 'rigid-body-2d+set-center-of-mass)

(defgproperty rigid-body-2d+inertia 'rigid-body-2d :get
 'rigid-body-2d+get-inertia :set 'rigid-body-2d+set-inertia)

(defgproperty rigid-body-2d+sleeping 'rigid-body-2d :get
 'rigid-body-2d+is-sleeping :set 'rigid-body-2d+set-sleeping)

(defgproperty rigid-body-2d+can-sleep 'rigid-body-2d :get
 'rigid-body-2d+is-able-to-sleep :set 'rigid-body-2d+set-can-sleep)

(defgproperty rigid-body-2d+lock-rotation 'rigid-body-2d :get
 'rigid-body-2d+is-lock-rotation-enabled :set
 'rigid-body-2d+set-lock-rotation-enabled)

(defgproperty rigid-body-2d+freeze 'rigid-body-2d :get
 'rigid-body-2d+is-freeze-enabled :set 'rigid-body-2d+set-freeze-enabled)

(defgproperty rigid-body-2d+freeze-mode 'rigid-body-2d :get
 'rigid-body-2d+get-freeze-mode :set 'rigid-body-2d+set-freeze-mode)

(defgproperty rigid-body-2d+custom-integrator 'rigid-body-2d :get
 'rigid-body-2d+is-using-custom-integrator :set
 'rigid-body-2d+set-use-custom-integrator)

(defgproperty rigid-body-2d+continuous-cd 'rigid-body-2d :get
 'rigid-body-2d+get-continuous-collision-detection-mode :set
 'rigid-body-2d+set-continuous-collision-detection-mode)

(defgproperty rigid-body-2d+contact-monitor 'rigid-body-2d :get
 'rigid-body-2d+is-contact-monitor-enabled :set
 'rigid-body-2d+set-contact-monitor)

(defgproperty rigid-body-2d+max-contacts-reported 'rigid-body-2d :get
 'rigid-body-2d+get-max-contacts-reported :set
 'rigid-body-2d+set-max-contacts-reported)

(defgproperty rigid-body-2d+linear-velocity 'rigid-body-2d :get
 'rigid-body-2d+get-linear-velocity :set 'rigid-body-2d+set-linear-velocity)

(defgproperty rigid-body-2d+linear-damp-mode 'rigid-body-2d :get
 'rigid-body-2d+get-linear-damp-mode :set 'rigid-body-2d+set-linear-damp-mode)

(defgproperty rigid-body-2d+linear-damp 'rigid-body-2d :get
 'rigid-body-2d+get-linear-damp :set 'rigid-body-2d+set-linear-damp)

(defgproperty rigid-body-2d+angular-velocity 'rigid-body-2d :get
 'rigid-body-2d+get-angular-velocity :set 'rigid-body-2d+set-angular-velocity)

(defgproperty rigid-body-2d+angular-damp-mode 'rigid-body-2d :get
 'rigid-body-2d+get-angular-damp-mode :set 'rigid-body-2d+set-angular-damp-mode)

(defgproperty rigid-body-2d+angular-damp 'rigid-body-2d :get
 'rigid-body-2d+get-angular-damp :set 'rigid-body-2d+set-angular-damp)

(defgproperty rigid-body-2d+constant-force 'rigid-body-2d :get
 'rigid-body-2d+get-constant-force :set 'rigid-body-2d+set-constant-force)

(defgproperty rigid-body-2d+constant-torque 'rigid-body-2d :get
 'rigid-body-2d+get-constant-torque :set 'rigid-body-2d+set-constant-torque)