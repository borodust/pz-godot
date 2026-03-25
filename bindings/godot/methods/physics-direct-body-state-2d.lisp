(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-body-state-2d+get-total-gravity :class
  'physics-direct-body-state-2d :bind "get_total_gravity" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-direct-body-state-2d+get-total-linear-damp :class
  'physics-direct-body-state-2d :bind "get_total_linear_damp" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+get-total-angular-damp :class
  'physics-direct-body-state-2d :bind "get_total_angular_damp" :hash
  1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+get-center-of-mass :class
  'physics-direct-body-state-2d :bind "get_center_of_mass" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-direct-body-state-2d+get-center-of-mass-local :class
  'physics-direct-body-state-2d :bind "get_center_of_mass_local" :hash
  3341600327)
 vector-2)

(defgmethod
 (physics-direct-body-state-2d+get-inverse-mass :class
  'physics-direct-body-state-2d :bind "get_inverse_mass" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+get-inverse-inertia :class
  'physics-direct-body-state-2d :bind "get_inverse_inertia" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+set-linear-velocity :class
  'physics-direct-body-state-2d :bind "set_linear_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (physics-direct-body-state-2d+get-linear-velocity :class
  'physics-direct-body-state-2d :bind "get_linear_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-direct-body-state-2d+set-angular-velocity :class
  'physics-direct-body-state-2d :bind "set_angular_velocity" :hash 373806689)
 :void (velocity float))

(defgmethod
 (physics-direct-body-state-2d+get-angular-velocity :class
  'physics-direct-body-state-2d :bind "get_angular_velocity" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+set-transform :class
  'physics-direct-body-state-2d :bind "set_transform" :hash 2761652528)
 :void (transform transform-2d))

(defgmethod
 (physics-direct-body-state-2d+get-transform :class
  'physics-direct-body-state-2d :bind "get_transform" :hash 3814499831)
 transform-2d)

(defgmethod
 (physics-direct-body-state-2d+get-velocity-at-local-position :class
  'physics-direct-body-state-2d :bind "get_velocity_at_local_position" :hash
  2656412154)
 vector-2 (local-position vector-2))

(defgmethod
 (physics-direct-body-state-2d+apply-central-impulse :class
  'physics-direct-body-state-2d :bind "apply_central_impulse" :hash 743155724)
 :void (impulse vector-2))

(defgmethod
 (physics-direct-body-state-2d+apply-torque-impulse :class
  'physics-direct-body-state-2d :bind "apply_torque_impulse" :hash 373806689)
 :void (impulse float))

(defgmethod
 (physics-direct-body-state-2d+apply-impulse :class
  'physics-direct-body-state-2d :bind "apply_impulse" :hash 4288681949)
 :void (impulse vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2d+apply-central-force :class
  'physics-direct-body-state-2d :bind "apply_central_force" :hash 3862383994)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2d+apply-force :class 'physics-direct-body-state-2d
  :bind "apply_force" :hash 4288681949)
 :void (force vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2d+apply-torque :class
  'physics-direct-body-state-2d :bind "apply_torque" :hash 373806689)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2d+add-constant-central-force :class
  'physics-direct-body-state-2d :bind "add_constant_central_force" :hash
  3862383994)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2d+add-constant-force :class
  'physics-direct-body-state-2d :bind "add_constant_force" :hash 4288681949)
 :void (force vector-2) (position vector-2))

(defgmethod
 (physics-direct-body-state-2d+add-constant-torque :class
  'physics-direct-body-state-2d :bind "add_constant_torque" :hash 373806689)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2d+set-constant-force :class
  'physics-direct-body-state-2d :bind "set_constant_force" :hash 743155724)
 :void (force vector-2))

(defgmethod
 (physics-direct-body-state-2d+get-constant-force :class
  'physics-direct-body-state-2d :bind "get_constant_force" :hash 3341600327)
 vector-2)

(defgmethod
 (physics-direct-body-state-2d+set-constant-torque :class
  'physics-direct-body-state-2d :bind "set_constant_torque" :hash 373806689)
 :void (torque float))

(defgmethod
 (physics-direct-body-state-2d+get-constant-torque :class
  'physics-direct-body-state-2d :bind "get_constant_torque" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+set-sleep-state :class
  'physics-direct-body-state-2d :bind "set_sleep_state" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (physics-direct-body-state-2d+is-sleeping :class 'physics-direct-body-state-2d
  :bind "is_sleeping" :hash 36873697)
 bool)

(defgmethod
 (physics-direct-body-state-2d+set-collision-layer :class
  'physics-direct-body-state-2d :bind "set_collision_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (physics-direct-body-state-2d+get-collision-layer :class
  'physics-direct-body-state-2d :bind "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-2d+set-collision-mask :class
  'physics-direct-body-state-2d :bind "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (physics-direct-body-state-2d+get-collision-mask :class
  'physics-direct-body-state-2d :bind "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-2d+get-contact-count :class
  'physics-direct-body-state-2d :bind "get_contact_count" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-2d+get-contact-local-position :class
  'physics-direct-body-state-2d :bind "get_contact_local_position" :hash
  2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-local-normal :class
  'physics-direct-body-state-2d :bind "get_contact_local_normal" :hash
  2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-local-shape :class
  'physics-direct-body-state-2d :bind "get_contact_local_shape" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-local-velocity-at-position :class
  'physics-direct-body-state-2d :bind "get_contact_local_velocity_at_position"
  :hash 2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider :class
  'physics-direct-body-state-2d :bind "get_contact_collider" :hash 495598643)
 rid (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider-position :class
  'physics-direct-body-state-2d :bind "get_contact_collider_position" :hash
  2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider-id :class
  'physics-direct-body-state-2d :bind "get_contact_collider_id" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider-object :class
  'physics-direct-body-state-2d :bind "get_contact_collider_object" :hash
  3332903315)
 object (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider-shape :class
  'physics-direct-body-state-2d :bind "get_contact_collider_shape" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-collider-velocity-at-position :class
  'physics-direct-body-state-2d :bind
  "get_contact_collider_velocity_at_position" :hash 2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-contact-impulse :class
  'physics-direct-body-state-2d :bind "get_contact_impulse" :hash 2299179447)
 vector-2 (contact-idx int))

(defgmethod
 (physics-direct-body-state-2d+get-step :class 'physics-direct-body-state-2d
  :bind "get_step" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-2d+integrate-forces :class
  'physics-direct-body-state-2d :bind "integrate_forces" :hash 3218959716)
 :void)

(defgmethod
 (physics-direct-body-state-2d+get-space-state :class
  'physics-direct-body-state-2d :bind "get_space_state" :hash 2506717822)
 physics-direct-space-state-2d)