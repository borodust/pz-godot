(common-lisp:in-package :%godot)


(defgproperty physical-bone-3d+joint-type 'physical-bone-3d :get
 'physical-bone-3d+get-joint-type :set 'physical-bone-3d+set-joint-type)

(defgproperty physical-bone-3d+joint-offset 'physical-bone-3d :get
 'physical-bone-3d+get-joint-offset :set 'physical-bone-3d+set-joint-offset)

(defgproperty physical-bone-3d+joint-rotation 'physical-bone-3d :get
 'physical-bone-3d+get-joint-rotation :set 'physical-bone-3d+set-joint-rotation)

(defgproperty physical-bone-3d+body-offset 'physical-bone-3d :get
 'physical-bone-3d+get-body-offset :set 'physical-bone-3d+set-body-offset)

(defgproperty physical-bone-3d+mass 'physical-bone-3d :get
 'physical-bone-3d+get-mass :set 'physical-bone-3d+set-mass)

(defgproperty physical-bone-3d+friction 'physical-bone-3d :get
 'physical-bone-3d+get-friction :set 'physical-bone-3d+set-friction)

(defgproperty physical-bone-3d+bounce 'physical-bone-3d :get
 'physical-bone-3d+get-bounce :set 'physical-bone-3d+set-bounce)

(defgproperty physical-bone-3d+gravity-scale 'physical-bone-3d :get
 'physical-bone-3d+get-gravity-scale :set 'physical-bone-3d+set-gravity-scale)

(defgproperty physical-bone-3d+custom-integrator 'physical-bone-3d :get
 'physical-bone-3d+is-using-custom-integrator :set
 'physical-bone-3d+set-use-custom-integrator)

(defgproperty physical-bone-3d+linear-damp-mode 'physical-bone-3d :get
 'physical-bone-3d+get-linear-damp-mode :set
 'physical-bone-3d+set-linear-damp-mode)

(defgproperty physical-bone-3d+linear-damp 'physical-bone-3d :get
 'physical-bone-3d+get-linear-damp :set 'physical-bone-3d+set-linear-damp)

(defgproperty physical-bone-3d+angular-damp-mode 'physical-bone-3d :get
 'physical-bone-3d+get-angular-damp-mode :set
 'physical-bone-3d+set-angular-damp-mode)

(defgproperty physical-bone-3d+angular-damp 'physical-bone-3d :get
 'physical-bone-3d+get-angular-damp :set 'physical-bone-3d+set-angular-damp)

(defgproperty physical-bone-3d+linear-velocity 'physical-bone-3d :get
 'physical-bone-3d+get-linear-velocity :set
 'physical-bone-3d+set-linear-velocity)

(defgproperty physical-bone-3d+angular-velocity 'physical-bone-3d :get
 'physical-bone-3d+get-angular-velocity :set
 'physical-bone-3d+set-angular-velocity)

(defgproperty physical-bone-3d+can-sleep 'physical-bone-3d :get
 'physical-bone-3d+is-able-to-sleep :set 'physical-bone-3d+set-can-sleep)