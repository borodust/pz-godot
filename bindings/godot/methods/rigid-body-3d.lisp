(common-lisp:in-package :%godot)


(defgmethod
 (rigid-body-3d+%integrate-forces :class 'rigid-body-3d :bind
  "_integrate_forces" :hash 420958145 :virtual common-lisp:t)
 :void (state physics-direct-body-state-3d))

(defgmethod
 (rigid-body-3d+set-mass :class 'rigid-body-3d :bind "set_mass" :hash
  373806689)
 :void (mass float))

(defgmethod
 (rigid-body-3d+get-mass :class 'rigid-body-3d :bind "get_mass" :hash
  1740695150)
 float)

(defgmethod
 (rigid-body-3d+set-inertia :class 'rigid-body-3d :bind "set_inertia" :hash
  3460891852)
 :void (inertia vector-3))

(defgmethod
 (rigid-body-3d+get-inertia :class 'rigid-body-3d :bind "get_inertia" :hash
  3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+set-center-of-mass-mode :class 'rigid-body-3d :bind
  "set_center_of_mass_mode" :hash 3625866032)
 :void (mode rigid-body-3d+center-of-mass-mode))

(defgmethod
 (rigid-body-3d+get-center-of-mass-mode :class 'rigid-body-3d :bind
  "get_center_of_mass_mode" :hash 237405040)
 rigid-body-3d+center-of-mass-mode)

(defgmethod
 (rigid-body-3d+set-center-of-mass :class 'rigid-body-3d :bind
  "set_center_of_mass" :hash 3460891852)
 :void (center-of-mass vector-3))

(defgmethod
 (rigid-body-3d+get-center-of-mass :class 'rigid-body-3d :bind
  "get_center_of_mass" :hash 3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+set-physics-material-override :class 'rigid-body-3d :bind
  "set_physics_material_override" :hash 1784508650)
 :void (physics-material-override physics-material))

(defgmethod
 (rigid-body-3d+get-physics-material-override :class 'rigid-body-3d :bind
  "get_physics_material_override" :hash 2521850424)
 physics-material)

(defgmethod
 (rigid-body-3d+set-linear-velocity :class 'rigid-body-3d :bind
  "set_linear_velocity" :hash 3460891852)
 :void (linear-velocity vector-3))

(defgmethod
 (rigid-body-3d+get-linear-velocity :class 'rigid-body-3d :bind
  "get_linear_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+set-angular-velocity :class 'rigid-body-3d :bind
  "set_angular_velocity" :hash 3460891852)
 :void (angular-velocity vector-3))

(defgmethod
 (rigid-body-3d+get-angular-velocity :class 'rigid-body-3d :bind
  "get_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+get-inverse-inertia-tensor :class 'rigid-body-3d :bind
  "get_inverse_inertia_tensor" :hash 2716978435)
 basis)

(defgmethod
 (rigid-body-3d+set-gravity-scale :class 'rigid-body-3d :bind
  "set_gravity_scale" :hash 373806689)
 :void (gravity-scale float))

(defgmethod
 (rigid-body-3d+get-gravity-scale :class 'rigid-body-3d :bind
  "get_gravity_scale" :hash 1740695150)
 float)

(defgmethod
 (rigid-body-3d+set-linear-damp-mode :class 'rigid-body-3d :bind
  "set_linear_damp_mode" :hash 1802035050)
 :void (linear-damp-mode rigid-body-3d+damp-mode))

(defgmethod
 (rigid-body-3d+get-linear-damp-mode :class 'rigid-body-3d :bind
  "get_linear_damp_mode" :hash 1366206940)
 rigid-body-3d+damp-mode)

(defgmethod
 (rigid-body-3d+set-angular-damp-mode :class 'rigid-body-3d :bind
  "set_angular_damp_mode" :hash 1802035050)
 :void (angular-damp-mode rigid-body-3d+damp-mode))

(defgmethod
 (rigid-body-3d+get-angular-damp-mode :class 'rigid-body-3d :bind
  "get_angular_damp_mode" :hash 1366206940)
 rigid-body-3d+damp-mode)

(defgmethod
 (rigid-body-3d+set-linear-damp :class 'rigid-body-3d :bind "set_linear_damp"
  :hash 373806689)
 :void (linear-damp float))

(defgmethod
 (rigid-body-3d+get-linear-damp :class 'rigid-body-3d :bind "get_linear_damp"
  :hash 1740695150)
 float)

(defgmethod
 (rigid-body-3d+set-angular-damp :class 'rigid-body-3d :bind "set_angular_damp"
  :hash 373806689)
 :void (angular-damp float))

(defgmethod
 (rigid-body-3d+get-angular-damp :class 'rigid-body-3d :bind "get_angular_damp"
  :hash 1740695150)
 float)

(defgmethod
 (rigid-body-3d+set-max-contacts-reported :class 'rigid-body-3d :bind
  "set_max_contacts_reported" :hash 1286410249)
 :void (amount int))

(defgmethod
 (rigid-body-3d+get-max-contacts-reported :class 'rigid-body-3d :bind
  "get_max_contacts_reported" :hash 3905245786)
 int)

(defgmethod
 (rigid-body-3d+get-contact-count :class 'rigid-body-3d :bind
  "get_contact_count" :hash 3905245786)
 int)

(defgmethod
 (rigid-body-3d+set-use-custom-integrator :class 'rigid-body-3d :bind
  "set_use_custom_integrator" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rigid-body-3d+is-using-custom-integrator :class 'rigid-body-3d :bind
  "is_using_custom_integrator" :hash 2240911060)
 bool)

(defgmethod
 (rigid-body-3d+set-contact-monitor :class 'rigid-body-3d :bind
  "set_contact_monitor" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rigid-body-3d+is-contact-monitor-enabled :class 'rigid-body-3d :bind
  "is_contact_monitor_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-use-continuous-collision-detection :class 'rigid-body-3d
  :bind "set_use_continuous_collision_detection" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rigid-body-3d+is-using-continuous-collision-detection :class 'rigid-body-3d
  :bind "is_using_continuous_collision_detection" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-axis-velocity :class 'rigid-body-3d :bind
  "set_axis_velocity" :hash 3460891852)
 :void (axis-velocity vector-3))

(defgmethod
 (rigid-body-3d+apply-central-impulse :class 'rigid-body-3d :bind
  "apply_central_impulse" :hash 3460891852)
 :void (impulse vector-3))

(defgmethod
 (rigid-body-3d+apply-impulse :class 'rigid-body-3d :bind "apply_impulse" :hash
  2754756483)
 :void (impulse vector-3) (position vector-3))

(defgmethod
 (rigid-body-3d+apply-torque-impulse :class 'rigid-body-3d :bind
  "apply_torque_impulse" :hash 3460891852)
 :void (impulse vector-3))

(defgmethod
 (rigid-body-3d+apply-central-force :class 'rigid-body-3d :bind
  "apply_central_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (rigid-body-3d+apply-force :class 'rigid-body-3d :bind "apply_force" :hash
  2754756483)
 :void (force vector-3) (position vector-3))

(defgmethod
 (rigid-body-3d+apply-torque :class 'rigid-body-3d :bind "apply_torque" :hash
  3460891852)
 :void (torque vector-3))

(defgmethod
 (rigid-body-3d+add-constant-central-force :class 'rigid-body-3d :bind
  "add_constant_central_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (rigid-body-3d+add-constant-force :class 'rigid-body-3d :bind
  "add_constant_force" :hash 2754756483)
 :void (force vector-3) (position vector-3))

(defgmethod
 (rigid-body-3d+add-constant-torque :class 'rigid-body-3d :bind
  "add_constant_torque" :hash 3460891852)
 :void (torque vector-3))

(defgmethod
 (rigid-body-3d+set-constant-force :class 'rigid-body-3d :bind
  "set_constant_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (rigid-body-3d+get-constant-force :class 'rigid-body-3d :bind
  "get_constant_force" :hash 3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+set-constant-torque :class 'rigid-body-3d :bind
  "set_constant_torque" :hash 3460891852)
 :void (torque vector-3))

(defgmethod
 (rigid-body-3d+get-constant-torque :class 'rigid-body-3d :bind
  "get_constant_torque" :hash 3360562783)
 vector-3)

(defgmethod
 (rigid-body-3d+set-sleeping :class 'rigid-body-3d :bind "set_sleeping" :hash
  2586408642)
 :void (sleeping bool))

(defgmethod
 (rigid-body-3d+is-sleeping :class 'rigid-body-3d :bind "is_sleeping" :hash
  36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-can-sleep :class 'rigid-body-3d :bind "set_can_sleep" :hash
  2586408642)
 :void (able-to-sleep bool))

(defgmethod
 (rigid-body-3d+is-able-to-sleep :class 'rigid-body-3d :bind "is_able_to_sleep"
  :hash 36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-lock-rotation-enabled :class 'rigid-body-3d :bind
  "set_lock_rotation_enabled" :hash 2586408642)
 :void (lock-rotation bool))

(defgmethod
 (rigid-body-3d+is-lock-rotation-enabled :class 'rigid-body-3d :bind
  "is_lock_rotation_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-freeze-enabled :class 'rigid-body-3d :bind
  "set_freeze_enabled" :hash 2586408642)
 :void (freeze-mode bool))

(defgmethod
 (rigid-body-3d+is-freeze-enabled :class 'rigid-body-3d :bind
  "is_freeze_enabled" :hash 36873697)
 bool)

(defgmethod
 (rigid-body-3d+set-freeze-mode :class 'rigid-body-3d :bind "set_freeze_mode"
  :hash 1319914653)
 :void (freeze-mode rigid-body-3d+freeze-mode))

(defgmethod
 (rigid-body-3d+get-freeze-mode :class 'rigid-body-3d :bind "get_freeze_mode"
  :hash 2008423905)
 rigid-body-3d+freeze-mode)

(defgmethod
 (rigid-body-3d+get-colliding-bodies :class 'rigid-body-3d :bind
  "get_colliding_bodies" :hash 3995934104)
 array)