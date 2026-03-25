(common-lisp:in-package :%godot)


(defgproperty rigid-body-3d+mass 'rigid-body-3d :get 'rigid-body-3d+get-mass
 :set 'rigid-body-3d+set-mass)

(defgproperty rigid-body-3d+physics-material-override 'rigid-body-3d :get
 'rigid-body-3d+get-physics-material-override :set
 'rigid-body-3d+set-physics-material-override)

(defgproperty rigid-body-3d+gravity-scale 'rigid-body-3d :get
 'rigid-body-3d+get-gravity-scale :set 'rigid-body-3d+set-gravity-scale)

(defgproperty rigid-body-3d+center-of-mass-mode 'rigid-body-3d :get
 'rigid-body-3d+get-center-of-mass-mode :set
 'rigid-body-3d+set-center-of-mass-mode)

(defgproperty rigid-body-3d+center-of-mass 'rigid-body-3d :get
 'rigid-body-3d+get-center-of-mass :set 'rigid-body-3d+set-center-of-mass)

(defgproperty rigid-body-3d+inertia 'rigid-body-3d :get
 'rigid-body-3d+get-inertia :set 'rigid-body-3d+set-inertia)

(defgproperty rigid-body-3d+sleeping 'rigid-body-3d :get
 'rigid-body-3d+is-sleeping :set 'rigid-body-3d+set-sleeping)

(defgproperty rigid-body-3d+can-sleep 'rigid-body-3d :get
 'rigid-body-3d+is-able-to-sleep :set 'rigid-body-3d+set-can-sleep)

(defgproperty rigid-body-3d+lock-rotation 'rigid-body-3d :get
 'rigid-body-3d+is-lock-rotation-enabled :set
 'rigid-body-3d+set-lock-rotation-enabled)

(defgproperty rigid-body-3d+freeze 'rigid-body-3d :get
 'rigid-body-3d+is-freeze-enabled :set 'rigid-body-3d+set-freeze-enabled)

(defgproperty rigid-body-3d+freeze-mode 'rigid-body-3d :get
 'rigid-body-3d+get-freeze-mode :set 'rigid-body-3d+set-freeze-mode)

(defgproperty rigid-body-3d+custom-integrator 'rigid-body-3d :get
 'rigid-body-3d+is-using-custom-integrator :set
 'rigid-body-3d+set-use-custom-integrator)

(defgproperty rigid-body-3d+continuous-cd 'rigid-body-3d :get
 'rigid-body-3d+is-using-continuous-collision-detection :set
 'rigid-body-3d+set-use-continuous-collision-detection)

(defgproperty rigid-body-3d+contact-monitor 'rigid-body-3d :get
 'rigid-body-3d+is-contact-monitor-enabled :set
 'rigid-body-3d+set-contact-monitor)

(defgproperty rigid-body-3d+max-contacts-reported 'rigid-body-3d :get
 'rigid-body-3d+get-max-contacts-reported :set
 'rigid-body-3d+set-max-contacts-reported)

(defgproperty rigid-body-3d+linear-velocity 'rigid-body-3d :get
 'rigid-body-3d+get-linear-velocity :set 'rigid-body-3d+set-linear-velocity)

(defgproperty rigid-body-3d+linear-damp-mode 'rigid-body-3d :get
 'rigid-body-3d+get-linear-damp-mode :set 'rigid-body-3d+set-linear-damp-mode)

(defgproperty rigid-body-3d+linear-damp 'rigid-body-3d :get
 'rigid-body-3d+get-linear-damp :set 'rigid-body-3d+set-linear-damp)

(defgproperty rigid-body-3d+angular-velocity 'rigid-body-3d :get
 'rigid-body-3d+get-angular-velocity :set 'rigid-body-3d+set-angular-velocity)

(defgproperty rigid-body-3d+angular-damp-mode 'rigid-body-3d :get
 'rigid-body-3d+get-angular-damp-mode :set 'rigid-body-3d+set-angular-damp-mode)

(defgproperty rigid-body-3d+angular-damp 'rigid-body-3d :get
 'rigid-body-3d+get-angular-damp :set 'rigid-body-3d+set-angular-damp)

(defgproperty rigid-body-3d+constant-force 'rigid-body-3d :get
 'rigid-body-3d+get-constant-force :set 'rigid-body-3d+set-constant-force)

(defgproperty rigid-body-3d+constant-torque 'rigid-body-3d :get
 'rigid-body-3d+get-constant-torque :set 'rigid-body-3d+set-constant-torque)