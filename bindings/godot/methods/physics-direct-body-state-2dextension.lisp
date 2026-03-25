(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-body-state-2dextension+-get-total-gravity :class
  'physics-direct-body-state-2dextension :bind "_get_total_gravity" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (physics-direct-body-state-2dextension+-get-total-linear-damp :class
  'physics-direct-body-state-2dextension :bind "_get_total_linear_damp" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-get-total-angular-damp :class
  'physics-direct-body-state-2dextension :bind "_get_total_angular_damp" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-get-center-of-mass :class
  'physics-direct-body-state-2dextension :bind "_get_center_of_mass" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (physics-direct-body-state-2dextension+-get-center-of-mass-local :class
  'physics-direct-body-state-2dextension :bind "_get_center_of_mass_local"
  :hash 3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (physics-direct-body-state-2dextension+-get-inverse-mass :class
  'physics-direct-body-state-2dextension :bind "_get_inverse_mass" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-get-inverse-inertia :class
  'physics-direct-body-state-2dextension :bind "_get_inverse_inertia" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-set-linear-velocity :class
  'physics-direct-body-state-2dextension :bind "_set_linear_velocity" :hash
  743155724 :virtual common-lisp:t)
 :void (velocity vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-get-linear-velocity :class
  'physics-direct-body-state-2dextension :bind "_get_linear_velocity" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (physics-direct-body-state-2dextension+-set-angular-velocity :class
  'physics-direct-body-state-2dextension :bind "_set_angular_velocity" :hash
  373806689 :virtual common-lisp:t)
 :void (velocity float))

(defgmethod
 (physics-direct-body-state-2dextension+-get-angular-velocity :class
  'physics-direct-body-state-2dextension :bind "_get_angular_velocity" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-set-transform :class
  'physics-direct-body-state-2dextension :bind "_set_transform" :hash
  2761652528 :virtual common-lisp:t)
 :void (transform transform-2d))

(defgmethod
 (physics-direct-body-state-2dextension+-get-transform :class
  'physics-direct-body-state-2dextension :bind "_get_transform" :hash
  3814499831 :virtual common-lisp:t)
 transform-2d)

(defgmethod
 (physics-direct-body-state-2dextension+-get-velocity-at-local-position :class
  'physics-direct-body-state-2dextension :bind
  "_get_velocity_at_local_position" :hash 2656412154 :virtual common-lisp:t)
 vector-2 (local-position vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-central-impulse :class
  'physics-direct-body-state-2dextension :bind "_apply_central_impulse" :hash
  743155724 :virtual common-lisp:t)
 :void (impulse vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-impulse :class
  'physics-direct-body-state-2dextension :bind "_apply_impulse" :hash
  3108078480 :virtual common-lisp:t)
 :void (impulse vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-torque-impulse :class
  'physics-direct-body-state-2dextension :bind "_apply_torque_impulse" :hash
  373806689 :virtual common-lisp:t)
 :void (impulse float))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-central-force :class
  'physics-direct-body-state-2dextension :bind "_apply_central_force" :hash
  743155724 :virtual common-lisp:t)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-force :class
  'physics-direct-body-state-2dextension :bind "_apply_force" :hash 3108078480
  :virtual common-lisp:t)
 :void (force vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-apply-torque :class
  'physics-direct-body-state-2dextension :bind "_apply_torque" :hash 373806689
  :virtual common-lisp:t)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2dextension+-add-constant-central-force :class
  'physics-direct-body-state-2dextension :bind "_add_constant_central_force"
  :hash 743155724 :virtual common-lisp:t)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-add-constant-force :class
  'physics-direct-body-state-2dextension :bind "_add_constant_force" :hash
  3108078480 :virtual common-lisp:t)
 :void (force vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-add-constant-torque :class
  'physics-direct-body-state-2dextension :bind "_add_constant_torque" :hash
  373806689 :virtual common-lisp:t)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2dextension+-set-constant-force :class
  'physics-direct-body-state-2dextension :bind "_set_constant_force" :hash
  743155724 :virtual common-lisp:t)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2dextension+-get-constant-force :class
  'physics-direct-body-state-2dextension :bind "_get_constant_force" :hash
  3341600327 :virtual common-lisp:t)
 vector-2)

(defgmethod
 (physics-direct-body-state-2dextension+-set-constant-torque :class
  'physics-direct-body-state-2dextension :bind "_set_constant_torque" :hash
  373806689 :virtual common-lisp:t)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2dextension+-get-constant-torque :class
  'physics-direct-body-state-2dextension :bind "_get_constant_torque" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-set-sleep-state :class
  'physics-direct-body-state-2dextension :bind "_set_sleep_state" :hash
  2586408642 :virtual common-lisp:t)
 :void (enabled bool))

(defgmethod
 (physics-direct-body-state-2dextension+-is-sleeping :class
  'physics-direct-body-state-2dextension :bind "_is_sleeping" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (physics-direct-body-state-2dextension+-set-collision-layer :class
  'physics-direct-body-state-2dextension :bind "_set_collision_layer" :hash
  1286410249 :virtual common-lisp:t)
 :void (layer int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-collision-layer :class
  'physics-direct-body-state-2dextension :bind "_get_collision_layer" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-2dextension+-set-collision-mask :class
  'physics-direct-body-state-2dextension :bind "_set_collision_mask" :hash
  1286410249 :virtual common-lisp:t)
 :void (mask int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-collision-mask :class
  'physics-direct-body-state-2dextension :bind "_get_collision_mask" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-count :class
  'physics-direct-body-state-2dextension :bind "_get_contact_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-local-position :class
  'physics-direct-body-state-2dextension :bind "_get_contact_local_position"
  :hash 2299179447 :virtual common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-local-normal :class
  'physics-direct-body-state-2dextension :bind "_get_contact_local_normal"
  :hash 2299179447 :virtual common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-local-shape :class
  'physics-direct-body-state-2dextension :bind "_get_contact_local_shape" :hash
  923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-local-velocity-at-position
  :class 'physics-direct-body-state-2dextension :bind
  "_get_contact_local_velocity_at_position" :hash 2299179447 :virtual
  common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider :class
  'physics-direct-body-state-2dextension :bind "_get_contact_collider" :hash
  495598643 :virtual common-lisp:t)
 rid (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider-position :class
  'physics-direct-body-state-2dextension :bind "_get_contact_collider_position"
  :hash 2299179447 :virtual common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider-id :class
  'physics-direct-body-state-2dextension :bind "_get_contact_collider_id" :hash
  923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider-object :class
  'physics-direct-body-state-2dextension :bind "_get_contact_collider_object"
  :hash 3332903315 :virtual common-lisp:t)
 object (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider-shape :class
  'physics-direct-body-state-2dextension :bind "_get_contact_collider_shape"
  :hash 923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-collider-velocity-at-position
  :class 'physics-direct-body-state-2dextension :bind
  "_get_contact_collider_velocity_at_position" :hash 2299179447 :virtual
  common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-contact-impulse :class
  'physics-direct-body-state-2dextension :bind "_get_contact_impulse" :hash
  2299179447 :virtual common-lisp:t)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2dextension+-get-step :class
  'physics-direct-body-state-2dextension :bind "_get_step" :hash 1740695150
  :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-2dextension+-integrate-forces :class
  'physics-direct-body-state-2dextension :bind "_integrate_forces" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-direct-body-state-2dextension+-get-space-state :class
  'physics-direct-body-state-2dextension :bind "_get_space_state" :hash
  2506717822 :virtual common-lisp:t)
 physics-direct-space-state-2d)