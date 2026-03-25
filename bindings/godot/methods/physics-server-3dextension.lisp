(common-lisp:in-package :%godot)


(defgmethod
 (physics-server-3dextension+-world-boundary-shape-create :class
  'physics-server-3dextension :bind "_world_boundary_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-separation-ray-shape-create :class
  'physics-server-3dextension :bind "_separation_ray_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-sphere-shape-create :class
  'physics-server-3dextension :bind "_sphere_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-box-shape-create :class
  'physics-server-3dextension :bind "_box_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-capsule-shape-create :class
  'physics-server-3dextension :bind "_capsule_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-cylinder-shape-create :class
  'physics-server-3dextension :bind "_cylinder_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-convex-polygon-shape-create :class
  'physics-server-3dextension :bind "_convex_polygon_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-concave-polygon-shape-create :class
  'physics-server-3dextension :bind "_concave_polygon_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-heightmap-shape-create :class
  'physics-server-3dextension :bind "_heightmap_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-custom-shape-create :class
  'physics-server-3dextension :bind "_custom_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-shape-set-data :class 'physics-server-3dextension
  :bind "_shape_set_data" :hash 3175752987 :virtual common-lisp:t)
 :void (shape rid) (data variant))

(defgmethod
 (physics-server-3dextension+-shape-set-custom-solver-bias :class
  'physics-server-3dextension :bind "_shape_set_custom_solver_bias" :hash
  1794382983 :virtual common-lisp:t)
 :void (shape rid) (bias float))

(defgmethod
 (physics-server-3dextension+-shape-set-margin :class
  'physics-server-3dextension :bind "_shape_set_margin" :hash 1794382983
  :virtual common-lisp:t)
 :void (shape rid) (margin float))

(defgmethod
 (physics-server-3dextension+-shape-get-margin :class
  'physics-server-3dextension :bind "_shape_get_margin" :hash 866169185
  :virtual common-lisp:t)
 float (shape rid))

(defgmethod
 (physics-server-3dextension+-shape-get-type :class 'physics-server-3dextension
  :bind "_shape_get_type" :hash 3418923367 :virtual common-lisp:t)
 physics-server-3d+shape-type (shape rid))

(defgmethod
 (physics-server-3dextension+-shape-get-data :class 'physics-server-3dextension
  :bind "_shape_get_data" :hash 4171304767 :virtual common-lisp:t)
 variant (shape rid))

(defgmethod
 (physics-server-3dextension+-shape-get-custom-solver-bias :class
  'physics-server-3dextension :bind "_shape_get_custom_solver_bias" :hash
  866169185 :virtual common-lisp:t)
 float (shape rid))

(defgmethod
 (physics-server-3dextension+-space-create :class 'physics-server-3dextension
  :bind "_space_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-space-set-active :class
  'physics-server-3dextension :bind "_space_set_active" :hash 1265174801
  :virtual common-lisp:t)
 :void (space rid) (active bool))

(defgmethod
 (physics-server-3dextension+-space-is-active :class
  'physics-server-3dextension :bind "_space_is_active" :hash 4155700596
  :virtual common-lisp:t)
 bool (space rid))

(defgmethod
 (physics-server-3dextension+-space-set-param :class
  'physics-server-3dextension :bind "_space_set_param" :hash 2406017470
  :virtual common-lisp:t)
 :void (space rid) (param physics-server-3d+space-parameter) (value float))

(defgmethod
 (physics-server-3dextension+-space-get-param :class
  'physics-server-3dextension :bind "_space_get_param" :hash 1523206731
  :virtual common-lisp:t)
 float (space rid) (param physics-server-3d+space-parameter))

(defgmethod
 (physics-server-3dextension+-space-get-direct-state :class
  'physics-server-3dextension :bind "_space_get_direct_state" :hash 2048616813
  :virtual common-lisp:t)
 physics-direct-space-state-3d (space rid))

(defgmethod
 (physics-server-3dextension+-space-set-debug-contacts :class
  'physics-server-3dextension :bind "_space_set_debug_contacts" :hash
  3411492887 :virtual common-lisp:t)
 :void (space rid) (max-contacts int))

(defgmethod
 (physics-server-3dextension+-space-get-contacts :class
  'physics-server-3dextension :bind "_space_get_contacts" :hash 808965560
  :virtual common-lisp:t)
 packed-vector-3array (space rid))

(defgmethod
 (physics-server-3dextension+-space-get-contact-count :class
  'physics-server-3dextension :bind "_space_get_contact_count" :hash 2198884583
  :virtual common-lisp:t)
 int (space rid))

(defgmethod
 (physics-server-3dextension+-area-create :class 'physics-server-3dextension
  :bind "_area_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-area-set-space :class 'physics-server-3dextension
  :bind "_area_set_space" :hash 395945892 :virtual common-lisp:t)
 :void (area rid) (space rid))

(defgmethod
 (physics-server-3dextension+-area-get-space :class 'physics-server-3dextension
  :bind "_area_get_space" :hash 3814569979 :virtual common-lisp:t)
 rid (area rid))

(defgmethod
 (physics-server-3dextension+-area-add-shape :class 'physics-server-3dextension
  :bind "_area_add_shape" :hash 2153848567 :virtual common-lisp:t)
 :void (area rid) (shape rid) (transform transform-3d) (disabled bool))

(defgmethod
 (physics-server-3dextension+-area-set-shape :class 'physics-server-3dextension
  :bind "_area_set_shape" :hash 2310537182 :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-3dextension+-area-set-shape-transform :class
  'physics-server-3dextension :bind "_area_set_shape_transform" :hash 675327471
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (transform transform-3d))

(defgmethod
 (physics-server-3dextension+-area-set-shape-disabled :class
  'physics-server-3dextension :bind "_area_set_shape_disabled" :hash 2658558584
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-3dextension+-area-get-shape-count :class
  'physics-server-3dextension :bind "_area_get_shape_count" :hash 2198884583
  :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-3dextension+-area-get-shape :class 'physics-server-3dextension
  :bind "_area_get_shape" :hash 1066463050 :virtual common-lisp:t)
 rid (area rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-area-get-shape-transform :class
  'physics-server-3dextension :bind "_area_get_shape_transform" :hash
  1050775521 :virtual common-lisp:t)
 transform-3d (area rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-area-remove-shape :class
  'physics-server-3dextension :bind "_area_remove_shape" :hash 3411492887
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-area-clear-shapes :class
  'physics-server-3dextension :bind "_area_clear_shapes" :hash 2722037293
  :virtual common-lisp:t)
 :void (area rid))

(defgmethod
 (physics-server-3dextension+-area-attach-object-instance-id :class
  'physics-server-3dextension :bind "_area_attach_object_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (area rid) (id int))

(defgmethod
 (physics-server-3dextension+-area-get-object-instance-id :class
  'physics-server-3dextension :bind "_area_get_object_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-3dextension+-area-set-param :class 'physics-server-3dextension
  :bind "_area_set_param" :hash 2980114638 :virtual common-lisp:t)
 :void (area rid) (param physics-server-3d+area-parameter) (value variant))

(defgmethod
 (physics-server-3dextension+-area-set-transform :class
  'physics-server-3dextension :bind "_area_set_transform" :hash 3935195649
  :virtual common-lisp:t)
 :void (area rid) (transform transform-3d))

(defgmethod
 (physics-server-3dextension+-area-get-param :class 'physics-server-3dextension
  :bind "_area_get_param" :hash 890056067 :virtual common-lisp:t)
 variant (area rid) (param physics-server-3d+area-parameter))

(defgmethod
 (physics-server-3dextension+-area-get-transform :class
  'physics-server-3dextension :bind "_area_get_transform" :hash 1128465797
  :virtual common-lisp:t)
 transform-3d (area rid))

(defgmethod
 (physics-server-3dextension+-area-set-collision-layer :class
  'physics-server-3dextension :bind "_area_set_collision_layer" :hash
  3411492887 :virtual common-lisp:t)
 :void (area rid) (layer int))

(defgmethod
 (physics-server-3dextension+-area-get-collision-layer :class
  'physics-server-3dextension :bind "_area_get_collision_layer" :hash
  2198884583 :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-3dextension+-area-set-collision-mask :class
  'physics-server-3dextension :bind "_area_set_collision_mask" :hash 3411492887
  :virtual common-lisp:t)
 :void (area rid) (mask int))

(defgmethod
 (physics-server-3dextension+-area-get-collision-mask :class
  'physics-server-3dextension :bind "_area_get_collision_mask" :hash 2198884583
  :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-3dextension+-area-set-monitorable :class
  'physics-server-3dextension :bind "_area_set_monitorable" :hash 1265174801
  :virtual common-lisp:t)
 :void (area rid) (monitorable bool))

(defgmethod
 (physics-server-3dextension+-area-set-ray-pickable :class
  'physics-server-3dextension :bind "_area_set_ray_pickable" :hash 1265174801
  :virtual common-lisp:t)
 :void (area rid) (enable bool))

(defgmethod
 (physics-server-3dextension+-area-set-monitor-callback :class
  'physics-server-3dextension :bind "_area_set_monitor_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-3dextension+-area-set-area-monitor-callback :class
  'physics-server-3dextension :bind "_area_set_area_monitor_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-3dextension+-body-create :class 'physics-server-3dextension
  :bind "_body_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-body-set-space :class 'physics-server-3dextension
  :bind "_body_set_space" :hash 395945892 :virtual common-lisp:t)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-3dextension+-body-get-space :class 'physics-server-3dextension
  :bind "_body_get_space" :hash 3814569979 :virtual common-lisp:t)
 rid (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-mode :class 'physics-server-3dextension
  :bind "_body_set_mode" :hash 606803466 :virtual common-lisp:t)
 :void (body rid) (mode physics-server-3d+body-mode))

(defgmethod
 (physics-server-3dextension+-body-get-mode :class 'physics-server-3dextension
  :bind "_body_get_mode" :hash 2488819728 :virtual common-lisp:t)
 physics-server-3d+body-mode (body rid))

(defgmethod
 (physics-server-3dextension+-body-add-shape :class 'physics-server-3dextension
  :bind "_body_add_shape" :hash 2153848567 :virtual common-lisp:t)
 :void (body rid) (shape rid) (transform transform-3d) (disabled bool))

(defgmethod
 (physics-server-3dextension+-body-set-shape :class 'physics-server-3dextension
  :bind "_body_set_shape" :hash 2310537182 :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-3dextension+-body-set-shape-transform :class
  'physics-server-3dextension :bind "_body_set_shape_transform" :hash 675327471
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (transform transform-3d))

(defgmethod
 (physics-server-3dextension+-body-set-shape-disabled :class
  'physics-server-3dextension :bind "_body_set_shape_disabled" :hash 2658558584
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-3dextension+-body-get-shape-count :class
  'physics-server-3dextension :bind "_body_get_shape_count" :hash 2198884583
  :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-get-shape :class 'physics-server-3dextension
  :bind "_body_get_shape" :hash 1066463050 :virtual common-lisp:t)
 rid (body rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-body-get-shape-transform :class
  'physics-server-3dextension :bind "_body_get_shape_transform" :hash
  1050775521 :virtual common-lisp:t)
 transform-3d (body rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-body-remove-shape :class
  'physics-server-3dextension :bind "_body_remove_shape" :hash 3411492887
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int))

(defgmethod
 (physics-server-3dextension+-body-clear-shapes :class
  'physics-server-3dextension :bind "_body_clear_shapes" :hash 2722037293
  :virtual common-lisp:t)
 :void (body rid))

(defgmethod
 (physics-server-3dextension+-body-attach-object-instance-id :class
  'physics-server-3dextension :bind "_body_attach_object_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (id int))

(defgmethod
 (physics-server-3dextension+-body-get-object-instance-id :class
  'physics-server-3dextension :bind "_body_get_object_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-enable-continuous-collision-detection
  :class 'physics-server-3dextension :bind
  "_body_set_enable_continuous_collision_detection" :hash 1265174801 :virtual
  common-lisp:t)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3dextension+-body-is-continuous-collision-detection-enabled
  :class 'physics-server-3dextension :bind
  "_body_is_continuous_collision_detection_enabled" :hash 4155700596 :virtual
  common-lisp:t)
 bool (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-collision-layer :class
  'physics-server-3dextension :bind "_body_set_collision_layer" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-3dextension+-body-get-collision-layer :class
  'physics-server-3dextension :bind "_body_get_collision_layer" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-collision-mask :class
  'physics-server-3dextension :bind "_body_set_collision_mask" :hash 3411492887
  :virtual common-lisp:t)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-3dextension+-body-get-collision-mask :class
  'physics-server-3dextension :bind "_body_get_collision_mask" :hash 2198884583
  :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-collision-priority :class
  'physics-server-3dextension :bind "_body_set_collision_priority" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (priority float))

(defgmethod
 (physics-server-3dextension+-body-get-collision-priority :class
  'physics-server-3dextension :bind "_body_get_collision_priority" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-user-flags :class
  'physics-server-3dextension :bind "_body_set_user_flags" :hash 3411492887
  :virtual common-lisp:t)
 :void (body rid) (flags int))

(defgmethod
 (physics-server-3dextension+-body-get-user-flags :class
  'physics-server-3dextension :bind "_body_get_user_flags" :hash 2198884583
  :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-param :class 'physics-server-3dextension
  :bind "_body_set_param" :hash 910941953 :virtual common-lisp:t)
 :void (body rid) (param physics-server-3d+body-parameter) (value variant))

(defgmethod
 (physics-server-3dextension+-body-get-param :class 'physics-server-3dextension
  :bind "_body_get_param" :hash 3385027841 :virtual common-lisp:t)
 variant (body rid) (param physics-server-3d+body-parameter))

(defgmethod
 (physics-server-3dextension+-body-reset-mass-properties :class
  'physics-server-3dextension :bind "_body_reset_mass_properties" :hash
  2722037293 :virtual common-lisp:t)
 :void (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-state :class 'physics-server-3dextension
  :bind "_body_set_state" :hash 599977762 :virtual common-lisp:t)
 :void (body rid) (state physics-server-3d+body-state) (value variant))

(defgmethod
 (physics-server-3dextension+-body-get-state :class 'physics-server-3dextension
  :bind "_body_get_state" :hash 1850449534 :virtual common-lisp:t)
 variant (body rid) (state physics-server-3d+body-state))

(defgmethod
 (physics-server-3dextension+-body-apply-central-impulse :class
  'physics-server-3dextension :bind "_body_apply_central_impulse" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3dextension+-body-apply-impulse :class
  'physics-server-3dextension :bind "_body_apply_impulse" :hash 3214966418
  :virtual common-lisp:t)
 :void (body rid) (impulse vector-3) (position vector-3))

(defgmethod
 (physics-server-3dextension+-body-apply-torque-impulse :class
  'physics-server-3dextension :bind "_body_apply_torque_impulse" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3dextension+-body-apply-central-force :class
  'physics-server-3dextension :bind "_body_apply_central_force" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3dextension+-body-apply-force :class
  'physics-server-3dextension :bind "_body_apply_force" :hash 3214966418
  :virtual common-lisp:t)
 :void (body rid) (force vector-3) (position vector-3))

(defgmethod
 (physics-server-3dextension+-body-apply-torque :class
  'physics-server-3dextension :bind "_body_apply_torque" :hash 3227306858
  :virtual common-lisp:t)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3dextension+-body-add-constant-central-force :class
  'physics-server-3dextension :bind "_body_add_constant_central_force" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3dextension+-body-add-constant-force :class
  'physics-server-3dextension :bind "_body_add_constant_force" :hash 3214966418
  :virtual common-lisp:t)
 :void (body rid) (force vector-3) (position vector-3))

(defgmethod
 (physics-server-3dextension+-body-add-constant-torque :class
  'physics-server-3dextension :bind "_body_add_constant_torque" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3dextension+-body-set-constant-force :class
  'physics-server-3dextension :bind "_body_set_constant_force" :hash 3227306858
  :virtual common-lisp:t)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3dextension+-body-get-constant-force :class
  'physics-server-3dextension :bind "_body_get_constant_force" :hash 531438156
  :virtual common-lisp:t)
 vector-3 (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-constant-torque :class
  'physics-server-3dextension :bind "_body_set_constant_torque" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3dextension+-body-get-constant-torque :class
  'physics-server-3dextension :bind "_body_get_constant_torque" :hash 531438156
  :virtual common-lisp:t)
 vector-3 (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-axis-velocity :class
  'physics-server-3dextension :bind "_body_set_axis_velocity" :hash 3227306858
  :virtual common-lisp:t)
 :void (body rid) (axis-velocity vector-3))

(defgmethod
 (physics-server-3dextension+-body-set-axis-lock :class
  'physics-server-3dextension :bind "_body_set_axis_lock" :hash 2020836892
  :virtual common-lisp:t)
 :void (body rid) (axis physics-server-3d+body-axis) (lock bool))

(defgmethod
 (physics-server-3dextension+-body-is-axis-locked :class
  'physics-server-3dextension :bind "_body_is_axis_locked" :hash 587853580
  :virtual common-lisp:t)
 bool (body rid) (axis physics-server-3d+body-axis))

(defgmethod
 (physics-server-3dextension+-body-add-collision-exception :class
  'physics-server-3dextension :bind "_body_add_collision_exception" :hash
  395945892 :virtual common-lisp:t)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-3dextension+-body-remove-collision-exception :class
  'physics-server-3dextension :bind "_body_remove_collision_exception" :hash
  395945892 :virtual common-lisp:t)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-3dextension+-body-get-collision-exceptions :class
  'physics-server-3dextension :bind "_body_get_collision_exceptions" :hash
  2684255073 :virtual common-lisp:t)
 array (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-max-contacts-reported :class
  'physics-server-3dextension :bind "_body_set_max_contacts_reported" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (amount int))

(defgmethod
 (physics-server-3dextension+-body-get-max-contacts-reported :class
  'physics-server-3dextension :bind "_body_get_max_contacts_reported" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-contacts-reported-depth-threshold :class
  'physics-server-3dextension :bind
  "_body_set_contacts_reported_depth_threshold" :hash 1794382983 :virtual
  common-lisp:t)
 :void (body rid) (threshold float))

(defgmethod
 (physics-server-3dextension+-body-get-contacts-reported-depth-threshold :class
  'physics-server-3dextension :bind
  "_body_get_contacts_reported_depth_threshold" :hash 866169185 :virtual
  common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-omit-force-integration :class
  'physics-server-3dextension :bind "_body_set_omit_force_integration" :hash
  1265174801 :virtual common-lisp:t)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3dextension+-body-is-omitting-force-integration :class
  'physics-server-3dextension :bind "_body_is_omitting_force_integration" :hash
  4155700596 :virtual common-lisp:t)
 bool (body rid))

(defgmethod
 (physics-server-3dextension+-body-set-state-sync-callback :class
  'physics-server-3dextension :bind "_body_set_state_sync_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (body rid) (callable callable))

(defgmethod
 (physics-server-3dextension+-body-set-force-integration-callback :class
  'physics-server-3dextension :bind "_body_set_force_integration_callback"
  :hash 2828036238 :virtual common-lisp:t)
 :void (body rid) (callable callable) (userdata variant))

(defgmethod
 (physics-server-3dextension+-body-set-ray-pickable :class
  'physics-server-3dextension :bind "_body_set_ray_pickable" :hash 1265174801
  :virtual common-lisp:t)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3dextension+-body-test-motion :class
  'physics-server-3dextension :bind "_body_test_motion" :hash 3627463434
  :virtual common-lisp:t)
 bool (body rid) (from transform-3d) (motion vector-3) (margin float)
 (max-collisions int) (collide-separation-ray bool)
 (recovery-as-collision bool)
 (result (:pointer physics-server-3dextension-motion-result)))

(defgmethod
 (physics-server-3dextension+-body-get-direct-state :class
  'physics-server-3dextension :bind "_body_get_direct_state" :hash 3029727957
  :virtual common-lisp:t)
 physics-direct-body-state-3d (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-create :class
  'physics-server-3dextension :bind "_soft_body_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-soft-body-update-rendering-server :class
  'physics-server-3dextension :bind "_soft_body_update_rendering_server" :hash
  2218179753 :virtual common-lisp:t)
 :void (body rid)
 (rendering-server-handler physics-server-3drendering-server-handler))

(defgmethod
 (physics-server-3dextension+-soft-body-set-space :class
  'physics-server-3dextension :bind "_soft_body_set_space" :hash 395945892
  :virtual common-lisp:t)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-3dextension+-soft-body-get-space :class
  'physics-server-3dextension :bind "_soft_body_get_space" :hash 3814569979
  :virtual common-lisp:t)
 rid (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-ray-pickable :class
  'physics-server-3dextension :bind "_soft_body_set_ray_pickable" :hash
  1265174801 :virtual common-lisp:t)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3dextension+-soft-body-set-collision-layer :class
  'physics-server-3dextension :bind "_soft_body_set_collision_layer" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-3dextension+-soft-body-get-collision-layer :class
  'physics-server-3dextension :bind "_soft_body_get_collision_layer" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-collision-mask :class
  'physics-server-3dextension :bind "_soft_body_set_collision_mask" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-3dextension+-soft-body-get-collision-mask :class
  'physics-server-3dextension :bind "_soft_body_get_collision_mask" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-add-collision-exception :class
  'physics-server-3dextension :bind "_soft_body_add_collision_exception" :hash
  395945892 :virtual common-lisp:t)
 :void (body rid) (body-b rid))

(defgmethod
 (physics-server-3dextension+-soft-body-remove-collision-exception :class
  'physics-server-3dextension :bind "_soft_body_remove_collision_exception"
  :hash 395945892 :virtual common-lisp:t)
 :void (body rid) (body-b rid))

(defgmethod
 (physics-server-3dextension+-soft-body-get-collision-exceptions :class
  'physics-server-3dextension :bind "_soft_body_get_collision_exceptions" :hash
  2684255073 :virtual common-lisp:t)
 array (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-state :class
  'physics-server-3dextension :bind "_soft_body_set_state" :hash 599977762
  :virtual common-lisp:t)
 :void (body rid) (state physics-server-3d+body-state) (variant variant))

(defgmethod
 (physics-server-3dextension+-soft-body-get-state :class
  'physics-server-3dextension :bind "_soft_body_get_state" :hash 1850449534
  :virtual common-lisp:t)
 variant (body rid) (state physics-server-3d+body-state))

(defgmethod
 (physics-server-3dextension+-soft-body-set-transform :class
  'physics-server-3dextension :bind "_soft_body_set_transform" :hash 3935195649
  :virtual common-lisp:t)
 :void (body rid) (transform transform-3d))

(defgmethod
 (physics-server-3dextension+-soft-body-set-simulation-precision :class
  'physics-server-3dextension :bind "_soft_body_set_simulation_precision" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (simulation-precision int))

(defgmethod
 (physics-server-3dextension+-soft-body-get-simulation-precision :class
  'physics-server-3dextension :bind "_soft_body_get_simulation_precision" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-total-mass :class
  'physics-server-3dextension :bind "_soft_body_set_total_mass" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (total-mass float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-total-mass :class
  'physics-server-3dextension :bind "_soft_body_get_total_mass" :hash 866169185
  :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-linear-stiffness :class
  'physics-server-3dextension :bind "_soft_body_set_linear_stiffness" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (linear-stiffness float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-linear-stiffness :class
  'physics-server-3dextension :bind "_soft_body_get_linear_stiffness" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-shrinking-factor :class
  'physics-server-3dextension :bind "_soft_body_set_shrinking_factor" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (shrinking-factor float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-shrinking-factor :class
  'physics-server-3dextension :bind "_soft_body_get_shrinking_factor" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-pressure-coefficient :class
  'physics-server-3dextension :bind "_soft_body_set_pressure_coefficient" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (pressure-coefficient float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-pressure-coefficient :class
  'physics-server-3dextension :bind "_soft_body_get_pressure_coefficient" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-damping-coefficient :class
  'physics-server-3dextension :bind "_soft_body_set_damping_coefficient" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (damping-coefficient float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-damping-coefficient :class
  'physics-server-3dextension :bind "_soft_body_get_damping_coefficient" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-drag-coefficient :class
  'physics-server-3dextension :bind "_soft_body_set_drag_coefficient" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (drag-coefficient float))

(defgmethod
 (physics-server-3dextension+-soft-body-get-drag-coefficient :class
  'physics-server-3dextension :bind "_soft_body_get_drag_coefficient" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-set-mesh :class
  'physics-server-3dextension :bind "_soft_body_set_mesh" :hash 395945892
  :virtual common-lisp:t)
 :void (body rid) (mesh rid))

(defgmethod
 (physics-server-3dextension+-soft-body-get-bounds :class
  'physics-server-3dextension :bind "_soft_body_get_bounds" :hash 974181306
  :virtual common-lisp:t)
 aabb (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-move-point :class
  'physics-server-3dextension :bind "_soft_body_move_point" :hash 831953689
  :virtual common-lisp:t)
 :void (body rid) (point-index int) (global-position vector-3))

(defgmethod
 (physics-server-3dextension+-soft-body-get-point-global-position :class
  'physics-server-3dextension :bind "_soft_body_get_point_global_position"
  :hash 3440143363 :virtual common-lisp:t)
 vector-3 (body rid) (point-index int))

(defgmethod
 (physics-server-3dextension+-soft-body-remove-all-pinned-points :class
  'physics-server-3dextension :bind "_soft_body_remove_all_pinned_points" :hash
  2722037293 :virtual common-lisp:t)
 :void (body rid))

(defgmethod
 (physics-server-3dextension+-soft-body-pin-point :class
  'physics-server-3dextension :bind "_soft_body_pin_point" :hash 2658558584
  :virtual common-lisp:t)
 :void (body rid) (point-index int) (pin bool))

(defgmethod
 (physics-server-3dextension+-soft-body-is-point-pinned :class
  'physics-server-3dextension :bind "_soft_body_is_point_pinned" :hash
  3120086654 :virtual common-lisp:t)
 bool (body rid) (point-index int))

(defgmethod
 (physics-server-3dextension+-soft-body-apply-point-impulse :class
  'physics-server-3dextension :bind "_soft_body_apply_point_impulse" :hash
  831953689 :virtual common-lisp:t)
 :void (body rid) (point-index int) (impulse vector-3))

(defgmethod
 (physics-server-3dextension+-soft-body-apply-point-force :class
  'physics-server-3dextension :bind "_soft_body_apply_point_force" :hash
  831953689 :virtual common-lisp:t)
 :void (body rid) (point-index int) (force vector-3))

(defgmethod
 (physics-server-3dextension+-soft-body-apply-central-impulse :class
  'physics-server-3dextension :bind "_soft_body_apply_central_impulse" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3dextension+-soft-body-apply-central-force :class
  'physics-server-3dextension :bind "_soft_body_apply_central_force" :hash
  3227306858 :virtual common-lisp:t)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3dextension+-joint-create :class 'physics-server-3dextension
  :bind "_joint_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-3dextension+-joint-clear :class 'physics-server-3dextension
  :bind "_joint_clear" :hash 2722037293 :virtual common-lisp:t)
 :void (joint rid))

(defgmethod
 (physics-server-3dextension+-joint-make-pin :class 'physics-server-3dextension
  :bind "_joint_make_pin" :hash 4280171926 :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (local-a vector-3) (body-b rid)
 (local-b vector-3))

(defgmethod
 (physics-server-3dextension+-pin-joint-set-param :class
  'physics-server-3dextension :bind "_pin_joint_set_param" :hash 810685294
  :virtual common-lisp:t)
 :void (joint rid) (param physics-server-3d+pin-joint-param) (value float))

(defgmethod
 (physics-server-3dextension+-pin-joint-get-param :class
  'physics-server-3dextension :bind "_pin_joint_get_param" :hash 2817972347
  :virtual common-lisp:t)
 float (joint rid) (param physics-server-3d+pin-joint-param))

(defgmethod
 (physics-server-3dextension+-pin-joint-set-local-a :class
  'physics-server-3dextension :bind "_pin_joint_set_local_a" :hash 3227306858
  :virtual common-lisp:t)
 :void (joint rid) (local-a vector-3))

(defgmethod
 (physics-server-3dextension+-pin-joint-get-local-a :class
  'physics-server-3dextension :bind "_pin_joint_get_local_a" :hash 531438156
  :virtual common-lisp:t)
 vector-3 (joint rid))

(defgmethod
 (physics-server-3dextension+-pin-joint-set-local-b :class
  'physics-server-3dextension :bind "_pin_joint_set_local_b" :hash 3227306858
  :virtual common-lisp:t)
 :void (joint rid) (local-b vector-3))

(defgmethod
 (physics-server-3dextension+-pin-joint-get-local-b :class
  'physics-server-3dextension :bind "_pin_joint_get_local_b" :hash 531438156
  :virtual common-lisp:t)
 vector-3 (joint rid))

(defgmethod
 (physics-server-3dextension+-joint-make-hinge :class
  'physics-server-3dextension :bind "_joint_make_hinge" :hash 1684107643
  :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (hinge-a transform-3d) (body-b rid)
 (hinge-b transform-3d))

(defgmethod
 (physics-server-3dextension+-joint-make-hinge-simple :class
  'physics-server-3dextension :bind "_joint_make_hinge_simple" :hash 4069547571
  :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (pivot-a vector-3) (axis-a vector-3)
 (body-b rid) (pivot-b vector-3) (axis-b vector-3))

(defgmethod
 (physics-server-3dextension+-hinge-joint-set-param :class
  'physics-server-3dextension :bind "_hinge_joint_set_param" :hash 3165502333
  :virtual common-lisp:t)
 :void (joint rid) (param physics-server-3d+hinge-joint-param) (value float))

(defgmethod
 (physics-server-3dextension+-hinge-joint-get-param :class
  'physics-server-3dextension :bind "_hinge_joint_get_param" :hash 2129207581
  :virtual common-lisp:t)
 float (joint rid) (param physics-server-3d+hinge-joint-param))

(defgmethod
 (physics-server-3dextension+-hinge-joint-set-flag :class
  'physics-server-3dextension :bind "_hinge_joint_set_flag" :hash 1601626188
  :virtual common-lisp:t)
 :void (joint rid) (flag physics-server-3d+hinge-joint-flag) (enabled bool))

(defgmethod
 (physics-server-3dextension+-hinge-joint-get-flag :class
  'physics-server-3dextension :bind "_hinge_joint_get_flag" :hash 4165147865
  :virtual common-lisp:t)
 bool (joint rid) (flag physics-server-3d+hinge-joint-flag))

(defgmethod
 (physics-server-3dextension+-joint-make-slider :class
  'physics-server-3dextension :bind "_joint_make_slider" :hash 1684107643
  :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3dextension+-slider-joint-set-param :class
  'physics-server-3dextension :bind "_slider_joint_set_param" :hash 2264833593
  :virtual common-lisp:t)
 :void (joint rid) (param physics-server-3d+slider-joint-param) (value float))

(defgmethod
 (physics-server-3dextension+-slider-joint-get-param :class
  'physics-server-3dextension :bind "_slider_joint_get_param" :hash 3498644957
  :virtual common-lisp:t)
 float (joint rid) (param physics-server-3d+slider-joint-param))

(defgmethod
 (physics-server-3dextension+-joint-make-cone-twist :class
  'physics-server-3dextension :bind "_joint_make_cone_twist" :hash 1684107643
  :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3dextension+-cone-twist-joint-set-param :class
  'physics-server-3dextension :bind "_cone_twist_joint_set_param" :hash
  808587618 :virtual common-lisp:t)
 :void (joint rid) (param physics-server-3d+cone-twist-joint-param)
 (value float))

(defgmethod
 (physics-server-3dextension+-cone-twist-joint-get-param :class
  'physics-server-3dextension :bind "_cone_twist_joint_get_param" :hash
  1134789658 :virtual common-lisp:t)
 float (joint rid) (param physics-server-3d+cone-twist-joint-param))

(defgmethod
 (physics-server-3dextension+-joint-make-generic-6dof :class
  'physics-server-3dextension :bind "_joint_make_generic_6dof" :hash 1684107643
  :virtual common-lisp:t)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3dextension+-generic-6dof-joint-set-param :class
  'physics-server-3dextension :bind "_generic_6dof_joint_set_param" :hash
  2600081391 :virtual common-lisp:t)
 :void (joint rid) (axis vector-3+axis)
 (param physics-server-3d+g6dofjoint-axis-param) (value float))

(defgmethod
 (physics-server-3dextension+-generic-6dof-joint-get-param :class
  'physics-server-3dextension :bind "_generic_6dof_joint_get_param" :hash
  467122058 :virtual common-lisp:t)
 float (joint rid) (axis vector-3+axis)
 (param physics-server-3d+g6dofjoint-axis-param))

(defgmethod
 (physics-server-3dextension+-generic-6dof-joint-set-flag :class
  'physics-server-3dextension :bind "_generic_6dof_joint_set_flag" :hash
  3570926903 :virtual common-lisp:t)
 :void (joint rid) (axis vector-3+axis)
 (flag physics-server-3d+g6dofjoint-axis-flag) (enable bool))

(defgmethod
 (physics-server-3dextension+-generic-6dof-joint-get-flag :class
  'physics-server-3dextension :bind "_generic_6dof_joint_get_flag" :hash
  4158090196 :virtual common-lisp:t)
 bool (joint rid) (axis vector-3+axis)
 (flag physics-server-3d+g6dofjoint-axis-flag))

(defgmethod
 (physics-server-3dextension+-joint-get-type :class 'physics-server-3dextension
  :bind "_joint_get_type" :hash 4290791900 :virtual common-lisp:t)
 physics-server-3d+joint-type (joint rid))

(defgmethod
 (physics-server-3dextension+-joint-set-solver-priority :class
  'physics-server-3dextension :bind "_joint_set_solver_priority" :hash
  3411492887 :virtual common-lisp:t)
 :void (joint rid) (priority int))

(defgmethod
 (physics-server-3dextension+-joint-get-solver-priority :class
  'physics-server-3dextension :bind "_joint_get_solver_priority" :hash
  2198884583 :virtual common-lisp:t)
 int (joint rid))

(defgmethod
 (physics-server-3dextension+-joint-disable-collisions-between-bodies :class
  'physics-server-3dextension :bind "_joint_disable_collisions_between_bodies"
  :hash 1265174801 :virtual common-lisp:t)
 :void (joint rid) (disable bool))

(defgmethod
 (physics-server-3dextension+-joint-is-disabled-collisions-between-bodies
  :class 'physics-server-3dextension :bind
  "_joint_is_disabled_collisions_between_bodies" :hash 4155700596 :virtual
  common-lisp:t)
 bool (joint rid))

(defgmethod
 (physics-server-3dextension+-free-rid :class 'physics-server-3dextension :bind
  "_free_rid" :hash 2722037293 :virtual common-lisp:t)
 :void (rid rid))

(defgmethod
 (physics-server-3dextension+-set-active :class 'physics-server-3dextension
  :bind "_set_active" :hash 2586408642 :virtual common-lisp:t)
 :void (active bool))

(defgmethod
 (physics-server-3dextension+-init :class 'physics-server-3dextension :bind
  "_init" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-3dextension+-step :class 'physics-server-3dextension :bind
  "_step" :hash 373806689 :virtual common-lisp:t)
 :void (step float))

(defgmethod
 (physics-server-3dextension+-sync :class 'physics-server-3dextension :bind
  "_sync" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-3dextension+-flush-queries :class 'physics-server-3dextension
  :bind "_flush_queries" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-3dextension+-end-sync :class 'physics-server-3dextension :bind
  "_end_sync" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-3dextension+-finish :class 'physics-server-3dextension :bind
  "_finish" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-3dextension+-is-flushing-queries :class
  'physics-server-3dextension :bind "_is_flushing_queries" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (physics-server-3dextension+-get-process-info :class
  'physics-server-3dextension :bind "_get_process_info" :hash 1332958745
  :virtual common-lisp:t)
 int (process-info physics-server-3d+process-info))

(defgmethod
 (physics-server-3dextension+body-test-motion-is-excluding-body :class
  'physics-server-3dextension :bind "body_test_motion_is_excluding_body" :hash
  4155700596)
 bool (body rid))

(defgmethod
 (physics-server-3dextension+body-test-motion-is-excluding-object :class
  'physics-server-3dextension :bind "body_test_motion_is_excluding_object"
  :hash 1116898809)
 bool (object int))