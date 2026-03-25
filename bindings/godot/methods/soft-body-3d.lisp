(common-lisp:in-package :%godot)


(defgmethod
 (soft-body-3d+get-physics-rid :class 'soft-body-3d :bind "get_physics_rid"
  :hash 2944877500)
 rid)

(defgmethod
 (soft-body-3d+set-collision-mask :class 'soft-body-3d :bind
  "set_collision_mask" :hash 1286410249)
 :void (collision-mask int))

(defgmethod
 (soft-body-3d+get-collision-mask :class 'soft-body-3d :bind
  "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (soft-body-3d+set-collision-layer :class 'soft-body-3d :bind
  "set_collision_layer" :hash 1286410249)
 :void (collision-layer int))

(defgmethod
 (soft-body-3d+get-collision-layer :class 'soft-body-3d :bind
  "get_collision_layer" :hash 3905245786)
 int)

(defgmethod
 (soft-body-3d+set-collision-mask-value :class 'soft-body-3d :bind
  "set_collision_mask_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (soft-body-3d+get-collision-mask-value :class 'soft-body-3d :bind
  "get_collision_mask_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (soft-body-3d+set-collision-layer-value :class 'soft-body-3d :bind
  "set_collision_layer_value" :hash 300928843)
 :void (layer-number int) (value bool))

(defgmethod
 (soft-body-3d+get-collision-layer-value :class 'soft-body-3d :bind
  "get_collision_layer_value" :hash 1116898809)
 bool (layer-number int))

(defgmethod
 (soft-body-3d+set-parent-collision-ignore :class 'soft-body-3d :bind
  "set_parent_collision_ignore" :hash 1348162250)
 :void (parent-collision-ignore node-path))

(defgmethod
 (soft-body-3d+get-parent-collision-ignore :class 'soft-body-3d :bind
  "get_parent_collision_ignore" :hash 4075236667)
 node-path)

(defgmethod
 (soft-body-3d+set-disable-mode :class 'soft-body-3d :bind "set_disable_mode"
  :hash 1104158384)
 :void (mode soft-body-3d+disable-mode))

(defgmethod
 (soft-body-3d+get-disable-mode :class 'soft-body-3d :bind "get_disable_mode"
  :hash 4135042476)
 soft-body-3d+disable-mode)

(defgmethod
 (soft-body-3d+get-collision-exceptions :class 'soft-body-3d :bind
  "get_collision_exceptions" :hash 2915620761)
 array)

(defgmethod
 (soft-body-3d+add-collision-exception-with :class 'soft-body-3d :bind
  "add_collision_exception_with" :hash 1078189570)
 :void (body node))

(defgmethod
 (soft-body-3d+remove-collision-exception-with :class 'soft-body-3d :bind
  "remove_collision_exception_with" :hash 1078189570)
 :void (body node))

(defgmethod
 (soft-body-3d+set-simulation-precision :class 'soft-body-3d :bind
  "set_simulation_precision" :hash 1286410249)
 :void (simulation-precision int))

(defgmethod
 (soft-body-3d+get-simulation-precision :class 'soft-body-3d :bind
  "get_simulation_precision" :hash 2455072627)
 int)

(defgmethod
 (soft-body-3d+set-total-mass :class 'soft-body-3d :bind "set_total_mass" :hash
  373806689)
 :void (mass float))

(defgmethod
 (soft-body-3d+get-total-mass :class 'soft-body-3d :bind "get_total_mass" :hash
  191475506)
 float)

(defgmethod
 (soft-body-3d+set-linear-stiffness :class 'soft-body-3d :bind
  "set_linear_stiffness" :hash 373806689)
 :void (linear-stiffness float))

(defgmethod
 (soft-body-3d+get-linear-stiffness :class 'soft-body-3d :bind
  "get_linear_stiffness" :hash 191475506)
 float)

(defgmethod
 (soft-body-3d+set-shrinking-factor :class 'soft-body-3d :bind
  "set_shrinking_factor" :hash 373806689)
 :void (shrinking-factor float))

(defgmethod
 (soft-body-3d+get-shrinking-factor :class 'soft-body-3d :bind
  "get_shrinking_factor" :hash 191475506)
 float)

(defgmethod
 (soft-body-3d+set-pressure-coefficient :class 'soft-body-3d :bind
  "set_pressure_coefficient" :hash 373806689)
 :void (pressure-coefficient float))

(defgmethod
 (soft-body-3d+get-pressure-coefficient :class 'soft-body-3d :bind
  "get_pressure_coefficient" :hash 191475506)
 float)

(defgmethod
 (soft-body-3d+set-damping-coefficient :class 'soft-body-3d :bind
  "set_damping_coefficient" :hash 373806689)
 :void (damping-coefficient float))

(defgmethod
 (soft-body-3d+get-damping-coefficient :class 'soft-body-3d :bind
  "get_damping_coefficient" :hash 191475506)
 float)

(defgmethod
 (soft-body-3d+set-drag-coefficient :class 'soft-body-3d :bind
  "set_drag_coefficient" :hash 373806689)
 :void (drag-coefficient float))

(defgmethod
 (soft-body-3d+get-drag-coefficient :class 'soft-body-3d :bind
  "get_drag_coefficient" :hash 191475506)
 float)

(defgmethod
 (soft-body-3d+get-point-transform :class 'soft-body-3d :bind
  "get_point_transform" :hash 871989493)
 vector-3 (point-index int))

(defgmethod
 (soft-body-3d+apply-impulse :class 'soft-body-3d :bind "apply_impulse" :hash
  1530502735)
 :void (point-index int) (impulse vector-3))

(defgmethod
 (soft-body-3d+apply-force :class 'soft-body-3d :bind "apply_force" :hash
  1530502735)
 :void (point-index int) (force vector-3))

(defgmethod
 (soft-body-3d+apply-central-impulse :class 'soft-body-3d :bind
  "apply_central_impulse" :hash 3460891852)
 :void (impulse vector-3))

(defgmethod
 (soft-body-3d+apply-central-force :class 'soft-body-3d :bind
  "apply_central_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (soft-body-3d+set-point-pinned :class 'soft-body-3d :bind "set_point_pinned"
  :hash 528784402)
 :void (point-index int) (pinned bool) (attachment-path node-path)
 (insert-at int))

(defgmethod
 (soft-body-3d+is-point-pinned :class 'soft-body-3d :bind "is_point_pinned"
  :hash 1116898809)
 bool (point-index int))

(defgmethod
 (soft-body-3d+set-ray-pickable :class 'soft-body-3d :bind "set_ray_pickable"
  :hash 2586408642)
 :void (ray-pickable bool))

(defgmethod
 (soft-body-3d+is-ray-pickable :class 'soft-body-3d :bind "is_ray_pickable"
  :hash 36873697)
 bool)