(common-lisp:in-package :%godot)


(defgmethod
 (physics-direct-body-state-3dextension+%get-total-gravity :class
  'physics-direct-body-state-3dextension :bind "_get_total_gravity" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%get-total-linear-damp :class
  'physics-direct-body-state-3dextension :bind "_get_total_linear_damp" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-3dextension+%get-total-angular-damp :class
  'physics-direct-body-state-3dextension :bind "_get_total_angular_damp" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-3dextension+%get-center-of-mass :class
  'physics-direct-body-state-3dextension :bind "_get_center_of_mass" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%get-center-of-mass-local :class
  'physics-direct-body-state-3dextension :bind "_get_center_of_mass_local"
  :hash 3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%get-principal-inertia-axes :class
  'physics-direct-body-state-3dextension :bind "_get_principal_inertia_axes"
  :hash 2716978435 :virtual common-lisp:t)
 basis)

(defgmethod
 (physics-direct-body-state-3dextension+%get-inverse-mass :class
  'physics-direct-body-state-3dextension :bind "_get_inverse_mass" :hash
  1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-3dextension+%get-inverse-inertia :class
  'physics-direct-body-state-3dextension :bind "_get_inverse_inertia" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%get-inverse-inertia-tensor :class
  'physics-direct-body-state-3dextension :bind "_get_inverse_inertia_tensor"
  :hash 2716978435 :virtual common-lisp:t)
 basis)

(defgmethod
 (physics-direct-body-state-3dextension+%set-linear-velocity :class
  'physics-direct-body-state-3dextension :bind "_set_linear_velocity" :hash
  3460891852 :virtual common-lisp:t)
 :void (velocity vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%get-linear-velocity :class
  'physics-direct-body-state-3dextension :bind "_get_linear_velocity" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%set-angular-velocity :class
  'physics-direct-body-state-3dextension :bind "_set_angular_velocity" :hash
  3460891852 :virtual common-lisp:t)
 :void (velocity vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%get-angular-velocity :class
  'physics-direct-body-state-3dextension :bind "_get_angular_velocity" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%set-transform :class
  'physics-direct-body-state-3dextension :bind "_set_transform" :hash
  2952846383 :virtual common-lisp:t)
 :void (transform transform-3d))

(defgmethod
 (physics-direct-body-state-3dextension+%get-transform :class
  'physics-direct-body-state-3dextension :bind "_get_transform" :hash
  3229777777 :virtual common-lisp:t)
 transform-3d)

(defgmethod
 (physics-direct-body-state-3dextension+%get-velocity-at-local-position :class
  'physics-direct-body-state-3dextension :bind
  "_get_velocity_at_local_position" :hash 192990374 :virtual common-lisp:t)
 vector-3 (local-position vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-central-impulse :class
  'physics-direct-body-state-3dextension :bind "_apply_central_impulse" :hash
  3460891852 :virtual common-lisp:t)
 :void (impulse vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-impulse :class
  'physics-direct-body-state-3dextension :bind "_apply_impulse" :hash
  1714681797 :virtual common-lisp:t)
 :void (impulse vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-torque-impulse :class
  'physics-direct-body-state-3dextension :bind "_apply_torque_impulse" :hash
  3460891852 :virtual common-lisp:t)
 :void (impulse vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-central-force :class
  'physics-direct-body-state-3dextension :bind "_apply_central_force" :hash
  3460891852 :virtual common-lisp:t)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-force :class
  'physics-direct-body-state-3dextension :bind "_apply_force" :hash 1714681797
  :virtual common-lisp:t)
 :void (force vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%apply-torque :class
  'physics-direct-body-state-3dextension :bind "_apply_torque" :hash 3460891852
  :virtual common-lisp:t)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%add-constant-central-force :class
  'physics-direct-body-state-3dextension :bind "_add_constant_central_force"
  :hash 3460891852 :virtual common-lisp:t)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%add-constant-force :class
  'physics-direct-body-state-3dextension :bind "_add_constant_force" :hash
  1714681797 :virtual common-lisp:t)
 :void (force vector-3) (position vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%add-constant-torque :class
  'physics-direct-body-state-3dextension :bind "_add_constant_torque" :hash
  3460891852 :virtual common-lisp:t)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%set-constant-force :class
  'physics-direct-body-state-3dextension :bind "_set_constant_force" :hash
  3460891852 :virtual common-lisp:t)
 :void (force vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%get-constant-force :class
  'physics-direct-body-state-3dextension :bind "_get_constant_force" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%set-constant-torque :class
  'physics-direct-body-state-3dextension :bind "_set_constant_torque" :hash
  3460891852 :virtual common-lisp:t)
 :void (torque vector-3))

(defgmethod
 (physics-direct-body-state-3dextension+%get-constant-torque :class
  'physics-direct-body-state-3dextension :bind "_get_constant_torque" :hash
  3360562783 :virtual common-lisp:t)
 vector-3)

(defgmethod
 (physics-direct-body-state-3dextension+%set-sleep-state :class
  'physics-direct-body-state-3dextension :bind "_set_sleep_state" :hash
  2586408642 :virtual common-lisp:t)
 :void (enabled bool))

(defgmethod
 (physics-direct-body-state-3dextension+%is-sleeping :class
  'physics-direct-body-state-3dextension :bind "_is_sleeping" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (physics-direct-body-state-3dextension+%set-collision-layer :class
  'physics-direct-body-state-3dextension :bind "_set_collision_layer" :hash
  1286410249 :virtual common-lisp:t)
 :void (layer int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-collision-layer :class
  'physics-direct-body-state-3dextension :bind "_get_collision_layer" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-3dextension+%set-collision-mask :class
  'physics-direct-body-state-3dextension :bind "_set_collision_mask" :hash
  1286410249 :virtual common-lisp:t)
 :void (mask int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-collision-mask :class
  'physics-direct-body-state-3dextension :bind "_get_collision_mask" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-count :class
  'physics-direct-body-state-3dextension :bind "_get_contact_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-local-position :class
  'physics-direct-body-state-3dextension :bind "_get_contact_local_position"
  :hash 711720468 :virtual common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-local-normal :class
  'physics-direct-body-state-3dextension :bind "_get_contact_local_normal"
  :hash 711720468 :virtual common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-impulse :class
  'physics-direct-body-state-3dextension :bind "_get_contact_impulse" :hash
  711720468 :virtual common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-local-shape :class
  'physics-direct-body-state-3dextension :bind "_get_contact_local_shape" :hash
  923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-local-velocity-at-position
  :class 'physics-direct-body-state-3dextension :bind
  "_get_contact_local_velocity_at_position" :hash 711720468 :virtual
  common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider :class
  'physics-direct-body-state-3dextension :bind "_get_contact_collider" :hash
  495598643 :virtual common-lisp:t)
 rid (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider-position :class
  'physics-direct-body-state-3dextension :bind "_get_contact_collider_position"
  :hash 711720468 :virtual common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider-id :class
  'physics-direct-body-state-3dextension :bind "_get_contact_collider_id" :hash
  923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider-object :class
  'physics-direct-body-state-3dextension :bind "_get_contact_collider_object"
  :hash 3332903315 :virtual common-lisp:t)
 object (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider-shape :class
  'physics-direct-body-state-3dextension :bind "_get_contact_collider_shape"
  :hash 923996154 :virtual common-lisp:t)
 int (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-contact-collider-velocity-at-position
  :class 'physics-direct-body-state-3dextension :bind
  "_get_contact_collider_velocity_at_position" :hash 711720468 :virtual
  common-lisp:t)
 vector-3 (contact-idx int))

(defgmethod
 (physics-direct-body-state-3dextension+%get-step :class
  'physics-direct-body-state-3dextension :bind "_get_step" :hash 1740695150
  :virtual common-lisp:t)
 float)

(defgmethod
 (physics-direct-body-state-3dextension+%integrate-forces :class
  'physics-direct-body-state-3dextension :bind "_integrate_forces" :hash
  3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-direct-body-state-3dextension+%get-space-state :class
  'physics-direct-body-state-3dextension :bind "_get_space_state" :hash
  2069328350 :virtual common-lisp:t)
 physics-direct-space-state-3d)