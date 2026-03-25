(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-body-state-3d+get-total-gravity :class
  'physics-direct-body-state-3d :bind "get_total_gravity" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+get-total-linear-damp :class
  'physics-direct-body-state-3d :bind "get_total_linear_damp" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-3d+get-total-angular-damp :class
  'physics-direct-body-state-3d :bind "get_total_angular_damp" :hash
  1740695150)
 float)

(defgmethod
 (physics-direct-body-state-3d+get-center-of-mass :class
  'physics-direct-body-state-3d :bind "get_center_of_mass" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+get-center-of-mass-local :class
  'physics-direct-body-state-3d :bind "get_center_of_mass_local" :hash
  3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+get-principal-inertia-axes :class
  'physics-direct-body-state-3d :bind "get_principal_inertia_axes" :hash
  2716978435)
 basis)

(defgmethod
 (physics-direct-body-state-3d+get-inverse-mass :class
  'physics-direct-body-state-3d :bind "get_inverse_mass" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-3d+get-inverse-inertia :class
  'physics-direct-body-state-3d :bind "get_inverse_inertia" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+get-inverse-inertia-tensor :class
  'physics-direct-body-state-3d :bind "get_inverse_inertia_tensor" :hash
  2716978435)
 basis)

(defgmethod
 (physics-direct-body-state-3d+set-linear-velocity :class
  'physics-direct-body-state-3d :bind "set_linear_velocity" :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (physics-direct-body-state-3d+get-linear-velocity :class
  'physics-direct-body-state-3d :bind "get_linear_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+set-angular-velocity :class
  'physics-direct-body-state-3d :bind "set_angular_velocity" :hash 3460891852)
 :void (velocity vector-3))

(defgmethod
 (physics-direct-body-state-3d+get-angular-velocity :class
  'physics-direct-body-state-3d :bind "get_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+set-transform :class
  'physics-direct-body-state-3d :bind "set_transform" :hash 2952846383)
 :void (transform transform-3d))

(defgmethod
 (physics-direct-body-state-3d+get-transform :class
  'physics-direct-body-state-3d :bind "get_transform" :hash 3229777777)
 transform-3d)

(defgmethod
 (physics-direct-body-state-3d+get-velocity-at-local-position :class
  'physics-direct-body-state-3d :bind "get_velocity_at_local_position" :hash
  192990374)
 vector-3 (local-position vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-central-impulse :class
  'physics-direct-body-state-3d :bind "apply_central_impulse" :hash 2007698547)
 :void (impulse vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-impulse :class
  'physics-direct-body-state-3d :bind "apply_impulse" :hash 2754756483)
 :void (impulse vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-torque-impulse :class
  'physics-direct-body-state-3d :bind "apply_torque_impulse" :hash 3460891852)
 :void (impulse vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-central-force :class
  'physics-direct-body-state-3d :bind "apply_central_force" :hash 2007698547)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-force :class 'physics-direct-body-state-3d
  :bind "apply_force" :hash 2754756483)
 :void (force vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3d+apply-torque :class
  'physics-direct-body-state-3d :bind "apply_torque" :hash 3460891852)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3d+add-constant-central-force :class
  'physics-direct-body-state-3d :bind "add_constant_central_force" :hash
  2007698547)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3d+add-constant-force :class
  'physics-direct-body-state-3d :bind "add_constant_force" :hash 2754756483)
 :void (force vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3d+add-constant-torque :class
  'physics-direct-body-state-3d :bind "add_constant_torque" :hash 3460891852)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3d+set-constant-force :class
  'physics-direct-body-state-3d :bind "set_constant_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3d+get-constant-force :class
  'physics-direct-body-state-3d :bind "get_constant_force" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+set-constant-torque :class
  'physics-direct-body-state-3d :bind "set_constant_torque" :hash 3460891852)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3d+get-constant-torque :class
  'physics-direct-body-state-3d :bind "get_constant_torque" :hash 3360562783)
 vector-3)

(defgmethod
 (physics-direct-body-state-3d+set-sleep-state :class
  'physics-direct-body-state-3d :bind "set_sleep_state" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (physics-direct-body-state-3d+is-sleeping :class 'physics-direct-body-state-3d
  :bind "is_sleeping" :hash 36873697)
 bool)

(defgmethod
 (physics-direct-body-state-3d+set-collision-layer :class
  'physics-direct-body-state-3d :bind "set_collision_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (physics-direct-body-state-3d+get-collision-layer :class
  'physics-direct-body-state-3d :bind "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-3d+set-collision-mask :class
  'physics-direct-body-state-3d :bind "set_collision_mask" :hash 1286410249)
 :void (mask int))

(defgmethod
 (physics-direct-body-state-3d+get-collision-mask :class
  'physics-direct-body-state-3d :bind "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-3d+get-contact-count :class
  'physics-direct-body-state-3d :bind "get_contact_count" :hash 3905245786)
 int)

(defgmethod
 (physics-direct-body-state-3d+get-contact-local-position :class
  'physics-direct-body-state-3d :bind "get_contact_local_position" :hash
  711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-local-normal :class
  'physics-direct-body-state-3d :bind "get_contact_local_normal" :hash
  711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-impulse :class
  'physics-direct-body-state-3d :bind "get_contact_impulse" :hash 711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-local-shape :class
  'physics-direct-body-state-3d :bind "get_contact_local_shape" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-local-velocity-at-position :class
  'physics-direct-body-state-3d :bind "get_contact_local_velocity_at_position"
  :hash 711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider :class
  'physics-direct-body-state-3d :bind "get_contact_collider" :hash 495598643)
 rid (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider-position :class
  'physics-direct-body-state-3d :bind "get_contact_collider_position" :hash
  711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider-id :class
  'physics-direct-body-state-3d :bind "get_contact_collider_id" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider-object :class
  'physics-direct-body-state-3d :bind "get_contact_collider_object" :hash
  3332903315)
 object (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider-shape :class
  'physics-direct-body-state-3d :bind "get_contact_collider_shape" :hash
  923996154)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-contact-collider-velocity-at-position :class
  'physics-direct-body-state-3d :bind
  "get_contact_collider_velocity_at_position" :hash 711720468)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3d+get-step :class 'physics-direct-body-state-3d
  :bind "get_step" :hash 1740695150)
 float)

(defgmethod
 (physics-direct-body-state-3d+integrate-forces :class
  'physics-direct-body-state-3d :bind "integrate_forces" :hash 3218959716)
 :void)

(defgmethod
 (physics-direct-body-state-3d+get-space-state :class
  'physics-direct-body-state-3d :bind "get_space_state" :hash 2069328350)
 physics-direct-space-state-3d)