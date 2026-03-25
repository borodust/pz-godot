(common-lisp:in-package :%godot)


(defgmethod
 (rigid-body-2d+-integrate-forces :class 'rigid-body-2d :bind
  "_integrate_forces" :hash 370287496 :virtual common-lisp:t)
 :void (state physics-direct-body-state-2d))

(defgmethod
 (rigid-body-2d+set-mass :class 'rigid-body-2d :bind "set_mass" :hash
  373806689)
 :void (mass float))

(defgmethod
 (rigid-body-2d+get-mass :class 'rigid-body-2d :bind "get_mass" :hash
  1740695150)
 float)

(defgmethod
 (rigid-body-2d+get-inertia :class 'rigid-body-2d :bind "get_inertia" :hash
  1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-inertia :class 'rigid-body-2d :bind "set_inertia" :hash
  373806689)
 :void (inertia float))

(defgmethod
 (rigid-body-2d+set-center-of-mass-mode :class 'rigid-body-2d :bind
  "set_center_of_mass_mode" :hash 1757235706)
 :void (mode rigid-body-2d+center-of-mass-mode))

(defgmethod
 (rigid-body-2d+get-center-of-mass-mode :class 'rigid-body-2d :bind
  "get_center_of_mass_mode" :hash 3277132817)
 rigid-body-2d+center-of-mass-mode)

(defgmethod
 (rigid-body-2d+set-center-of-mass :class 'rigid-body-2d :bind
  "set_center_of_mass" :hash 743155724)
 :void (center-of-mass vector-2))

(defgmethod
 (rigid-body-2d+get-center-of-mass :class 'rigid-body-2d :bind
  "get_center_of_mass" :hash 3341600327)
 vector-2)

(defgmethod
 (rigid-body-2d+set-physics-material-override :class 'rigid-body-2d :bind
  "set_physics_material_override" :hash 1784508650)
 :void (physics-material-override physics-material))

(defgmethod
 (rigid-body-2d+get-physics-material-override :class 'rigid-body-2d :bind
  "get_physics_material_override" :hash 2521850424)
 physics-material)

(defgmethod
 (rigid-body-2d+set-gravity-scale :class 'rigid-body-2d :bind
  "set_gravity_scale" :hash 373806689)
 :void (gravity-scale float))

(defgmethod
 (rigid-body-2d+get-gravity-scale :class 'rigid-body-2d :bind
  "get_gravity_scale" :hash 1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-linear-damp-mode :class 'rigid-body-2d :bind
  "set_linear_damp_mode" :hash 3406533708)
 :void (linear-damp-mode rigid-body-2d+damp-mode))

(defgmethod
 (rigid-body-2d+get-linear-damp-mode :class 'rigid-body-2d :bind
  "get_linear_damp_mode" :hash 2970511462)
 rigid-body-2d+damp-mode)

(defgmethod
 (rigid-body-2d+set-angular-damp-mode :class 'rigid-body-2d :bind
  "set_angular_damp_mode" :hash 3406533708)
 :void (angular-damp-mode rigid-body-2d+damp-mode))

(defgmethod
 (rigid-body-2d+get-angular-damp-mode :class 'rigid-body-2d :bind
  "get_angular_damp_mode" :hash 2970511462)
 rigid-body-2d+damp-mode)

(defgmethod
 (rigid-body-2d+set-linear-damp :class 'rigid-body-2d :bind "set_linear_damp"
  :hash 373806689)
 :void (linear-damp float))

(defgmethod
 (rigid-body-2d+get-linear-damp :class 'rigid-body-2d :bind "get_linear_damp"
  :hash 1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-angular-damp :class 'rigid-body-2d :bind "set_angular_damp"
  :hash 373806689)
 :void (angular-damp float))

(defgmethod
 (rigid-body-2d+get-angular-damp :class 'rigid-body-2d :bind "get_angular_damp"
  :hash 1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-linear-velocity :class 'rigid-body-2d :bind
  "set_linear_velocity" :hash 743155724)
 :void (linear-velocity vector-2))

(defgmethod
 (rigid-body-2d+get-linear-velocity :class 'rigid-body-2d :bind
  "get_linear_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (rigid-body-2d+set-angular-velocity :class 'rigid-body-2d :bind
  "set_angular_velocity" :hash 373806689)
 :void (angular-velocity float))

(defgmethod
 (rigid-body-2d+get-angular-velocity :class 'rigid-body-2d :bind
  "get_angular_velocity" :hash 1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-max-contacts-reported :class 'rigid-body-2d :bind
  "set_max_contacts_reported" :hash 1286410249)
 :void (amount int))

(defgmethod
 (rigid-body-2d+get-max-contacts-reported :class 'rigid-body-2d :bind
  "get_max_contacts_reported" :hash 3905245786)
 int)

(defgmethod
 (rigid-body-2d+get-contact-count :class 'rigid-body-2d :bind
  "get_contact_count" :hash 3905245786)
 int)

(defgmethod
 (rigid-body-2d+set-use-custom-integrator :class 'rigid-body-2d :bind
  "set_use_custom_integrator" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rigid-body-2d+is-using-custom-integrator :class 'rigid-body-2d :bind
  "is_using_custom_integrator" :hash 2240911060)
 bool)

(defgmethod
 (rigid-body-2d+set-contact-monitor :class 'rigid-body-2d :bind
  "set_contact_monitor" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rigid-body-2d+is-contact-monitor-enabled :class 'rigid-body-2d :bind
  "is_contact_monitor_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-2d+set-continuous-collision-detection-mode :class 'rigid-body-2d
  :bind "set_continuous_collision_detection_mode" :hash 1000241384)
 :void (mode rigid-body-2d+ccdmode))

(defgmethod
 (rigid-body-2d+get-continuous-collision-detection-mode :class 'rigid-body-2d
  :bind "get_continuous_collision_detection_mode" :hash 815214376)
 rigid-body-2d+ccdmode)

(defgmethod
 (rigid-body-2d+set-axis-velocity :class 'rigid-body-2d :bind
  "set_axis_velocity" :hash 743155724)
 :void (axis-velocity vector-2))

(defgmethod
 (rigid-body-2d+apply-central-impulse :class 'rigid-body-2d :bind
  "apply_central_impulse" :hash 3862383994)
 :void (impulse vector-2))

(defgmethod
 (rigid-body-2d+apply-impulse :class 'rigid-body-2d :bind "apply_impulse" :hash
  4288681949)
 :void (impulse vector-2) (position vector-2))

(defgmethod
 (rigid-body-2d+apply-torque-impulse :class 'rigid-body-2d :bind
  "apply_torque_impulse" :hash 373806689)
 :void (torque float))

(defgmethod
 (rigid-body-2d+apply-central-force :class 'rigid-body-2d :bind
  "apply_central_force" :hash 743155724)
 :void (force vector-2))

(defgmethod
 (rigid-body-2d+apply-force :class 'rigid-body-2d :bind "apply_force" :hash
  4288681949)
 :void (force vector-2) (position vector-2))

(defgmethod
 (rigid-body-2d+apply-torque :class 'rigid-body-2d :bind "apply_torque" :hash
  373806689)
 :void (torque float))

(defgmethod
 (rigid-body-2d+add-constant-central-force :class 'rigid-body-2d :bind
  "add_constant_central_force" :hash 743155724)
 :void (force vector-2))

(defgmethod
 (rigid-body-2d+add-constant-force :class 'rigid-body-2d :bind
  "add_constant_force" :hash 4288681949)
 :void (force vector-2) (position vector-2))

(defgmethod
 (rigid-body-2d+add-constant-torque :class 'rigid-body-2d :bind
  "add_constant_torque" :hash 373806689)
 :void (torque float))

(defgmethod
 (rigid-body-2d+set-constant-force :class 'rigid-body-2d :bind
  "set_constant_force" :hash 743155724)
 :void (force vector-2))

(defgmethod
 (rigid-body-2d+get-constant-force :class 'rigid-body-2d :bind
  "get_constant_force" :hash 3341600327)
 vector-2)

(defgmethod
 (rigid-body-2d+set-constant-torque :class 'rigid-body-2d :bind
  "set_constant_torque" :hash 373806689)
 :void (torque float))

(defgmethod
 (rigid-body-2d+get-constant-torque :class 'rigid-body-2d :bind
  "get_constant_torque" :hash 1740695150)
 float)

(defgmethod
 (rigid-body-2d+set-sleeping :class 'rigid-body-2d :bind "set_sleeping" :hash
  2586408642)
 :void (sleeping bool))

(defgmethod
 (rigid-body-2d+is-sleeping :class 'rigid-body-2d :bind "is_sleeping" :hash
  36873697)
 bool)

(defgmethod
 (rigid-body-2d+set-can-sleep :class 'rigid-body-2d :bind "set_can_sleep" :hash
  2586408642)
 :void (able-to-sleep bool))

(defgmethod
 (rigid-body-2d+is-able-to-sleep :class 'rigid-body-2d :bind "is_able_to_sleep"
  :hash 36873697)
 bool)

(defgmethod
 (rigid-body-2d+set-lock-rotation-enabled :class 'rigid-body-2d :bind
  "set_lock_rotation_enabled" :hash 2586408642)
 :void (lock-rotation bool))

(defgmethod
 (rigid-body-2d+is-lock-rotation-enabled :class 'rigid-body-2d :bind
  "is_lock_rotation_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-2d+set-freeze-enabled :class 'rigid-body-2d :bind
  "set_freeze_enabled" :hash 2586408642)
 :void (freeze-mode bool))

(defgmethod
 (rigid-body-2d+is-freeze-enabled :class 'rigid-body-2d :bind
  "is_freeze_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-2d+set-freeze-mode :class 'rigid-body-2d :bind "set_freeze_mode"
  :hash 1705112154)
 :void (freeze-mode rigid-body-2d+freeze-mode))

(defgmethod
 (rigid-body-2d+get-freeze-mode :class 'rigid-body-2d :bind "get_freeze_mode"
  :hash 2016872314)
 rigid-body-2d+freeze-mode)

(defgmethod
 (rigid-body-2d+get-colliding-bodies :class 'rigid-body-2d :bind
  "get_colliding_bodies" :hash 3995934104)
 array)