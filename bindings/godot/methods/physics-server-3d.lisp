(common-lisp:in-package :%godot)


(defgmethod
 (physics-server-3d+world-boundary-shape-create :class 'physics-server-3d :bind
  "world_boundary_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+separation-ray-shape-create :class 'physics-server-3d :bind
  "separation_ray_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+sphere-shape-create :class 'physics-server-3d :bind
  "sphere_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+box-shape-create :class 'physics-server-3d :bind
  "box_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+capsule-shape-create :class 'physics-server-3d :bind
  "capsule_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+cylinder-shape-create :class 'physics-server-3d :bind
  "cylinder_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+convex-polygon-shape-create :class 'physics-server-3d :bind
  "convex_polygon_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+concave-polygon-shape-create :class 'physics-server-3d
  :bind "concave_polygon_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+heightmap-shape-create :class 'physics-server-3d :bind
  "heightmap_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+custom-shape-create :class 'physics-server-3d :bind
  "custom_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+shape-set-data :class 'physics-server-3d :bind
  "shape_set_data" :hash 3175752987)
 :void (shape rid) (data variant))

(defgmethod
 (physics-server-3d+shape-set-margin :class 'physics-server-3d :bind
  "shape_set_margin" :hash 1794382983)
 :void (shape rid) (margin float))

(defgmethod
 (physics-server-3d+shape-get-type :class 'physics-server-3d :bind
  "shape_get_type" :hash 3418923367)
 physics-server-3d+shape-type (shape rid))

(defgmethod
 (physics-server-3d+shape-get-data :class 'physics-server-3d :bind
  "shape_get_data" :hash 4171304767)
 variant (shape rid))

(defgmethod
 (physics-server-3d+shape-get-margin :class 'physics-server-3d :bind
  "shape_get_margin" :hash 866169185)
 float (shape rid))

(defgmethod
 (physics-server-3d+space-create :class 'physics-server-3d :bind "space_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+space-set-active :class 'physics-server-3d :bind
  "space_set_active" :hash 1265174801)
 :void (space rid) (active bool))

(defgmethod
 (physics-server-3d+space-is-active :class 'physics-server-3d :bind
  "space_is_active" :hash 4155700596)
 bool (space rid))

(defgmethod
 (physics-server-3d+space-set-param :class 'physics-server-3d :bind
  "space_set_param" :hash 2406017470)
 :void (space rid) (param physics-server-3d+space-parameter) (value float))

(defgmethod
 (physics-server-3d+space-get-param :class 'physics-server-3d :bind
  "space_get_param" :hash 1523206731)
 float (space rid) (param physics-server-3d+space-parameter))

(defgmethod
 (physics-server-3d+space-get-direct-state :class 'physics-server-3d :bind
  "space_get_direct_state" :hash 2048616813)
 physics-direct-space-state-3d (space rid))

(defgmethod
 (physics-server-3d+area-create :class 'physics-server-3d :bind "area_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+area-set-space :class 'physics-server-3d :bind
  "area_set_space" :hash 395945892)
 :void (area rid) (space rid))

(defgmethod
 (physics-server-3d+area-get-space :class 'physics-server-3d :bind
  "area_get_space" :hash 3814569979)
 rid (area rid))

(defgmethod
 (physics-server-3d+area-add-shape :class 'physics-server-3d :bind
  "area_add_shape" :hash 3711419014)
 :void (area rid) (shape rid) (transform transform-3d) (disabled bool))

(defgmethod
 (physics-server-3d+area-set-shape :class 'physics-server-3d :bind
  "area_set_shape" :hash 2310537182)
 :void (area rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-3d+area-set-shape-transform :class 'physics-server-3d :bind
  "area_set_shape_transform" :hash 675327471)
 :void (area rid) (shape-idx int) (transform transform-3d))

(defgmethod
 (physics-server-3d+area-set-shape-disabled :class 'physics-server-3d :bind
  "area_set_shape_disabled" :hash 2658558584)
 :void (area rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-3d+area-get-shape-count :class 'physics-server-3d :bind
  "area_get_shape_count" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-3d+area-get-shape :class 'physics-server-3d :bind
  "area_get_shape" :hash 1066463050)
 rid (area rid) (shape-idx int))

(defgmethod
 (physics-server-3d+area-get-shape-transform :class 'physics-server-3d :bind
  "area_get_shape_transform" :hash 1050775521)
 transform-3d (area rid) (shape-idx int))

(defgmethod
 (physics-server-3d+area-remove-shape :class 'physics-server-3d :bind
  "area_remove_shape" :hash 3411492887)
 :void (area rid) (shape-idx int))

(defgmethod
 (physics-server-3d+area-clear-shapes :class 'physics-server-3d :bind
  "area_clear_shapes" :hash 2722037293)
 :void (area rid))

(defgmethod
 (physics-server-3d+area-set-collision-layer :class 'physics-server-3d :bind
  "area_set_collision_layer" :hash 3411492887)
 :void (area rid) (layer int))

(defgmethod
 (physics-server-3d+area-get-collision-layer :class 'physics-server-3d :bind
  "area_get_collision_layer" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-3d+area-set-collision-mask :class 'physics-server-3d :bind
  "area_set_collision_mask" :hash 3411492887)
 :void (area rid) (mask int))

(defgmethod
 (physics-server-3d+area-get-collision-mask :class 'physics-server-3d :bind
  "area_get_collision_mask" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-3d+area-set-param :class 'physics-server-3d :bind
  "area_set_param" :hash 2980114638)
 :void (area rid) (param physics-server-3d+area-parameter) (value variant))

(defgmethod
 (physics-server-3d+area-set-transform :class 'physics-server-3d :bind
  "area_set_transform" :hash 3935195649)
 :void (area rid) (transform transform-3d))

(defgmethod
 (physics-server-3d+area-get-param :class 'physics-server-3d :bind
  "area_get_param" :hash 890056067)
 variant (area rid) (param physics-server-3d+area-parameter))

(defgmethod
 (physics-server-3d+area-get-transform :class 'physics-server-3d :bind
  "area_get_transform" :hash 1128465797)
 transform-3d (area rid))

(defgmethod
 (physics-server-3d+area-attach-object-instance-id :class 'physics-server-3d
  :bind "area_attach_object_instance_id" :hash 3411492887)
 :void (area rid) (id int))

(defgmethod
 (physics-server-3d+area-get-object-instance-id :class 'physics-server-3d :bind
  "area_get_object_instance_id" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-3d+area-set-monitor-callback :class 'physics-server-3d :bind
  "area_set_monitor_callback" :hash 3379118538)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-3d+area-set-area-monitor-callback :class 'physics-server-3d
  :bind "area_set_area_monitor_callback" :hash 3379118538)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-3d+area-set-monitorable :class 'physics-server-3d :bind
  "area_set_monitorable" :hash 1265174801)
 :void (area rid) (monitorable bool))

(defgmethod
 (physics-server-3d+area-set-ray-pickable :class 'physics-server-3d :bind
  "area_set_ray_pickable" :hash 1265174801)
 :void (area rid) (enable bool))

(defgmethod
 (physics-server-3d+body-create :class 'physics-server-3d :bind "body_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+body-set-space :class 'physics-server-3d :bind
  "body_set_space" :hash 395945892)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-3d+body-get-space :class 'physics-server-3d :bind
  "body_get_space" :hash 3814569979)
 rid (body rid))

(defgmethod
 (physics-server-3d+body-set-mode :class 'physics-server-3d :bind
  "body_set_mode" :hash 606803466)
 :void (body rid) (mode physics-server-3d+body-mode))

(defgmethod
 (physics-server-3d+body-get-mode :class 'physics-server-3d :bind
  "body_get_mode" :hash 2488819728)
 physics-server-3d+body-mode (body rid))

(defgmethod
 (physics-server-3d+body-set-collision-layer :class 'physics-server-3d :bind
  "body_set_collision_layer" :hash 3411492887)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-3d+body-get-collision-layer :class 'physics-server-3d :bind
  "body_get_collision_layer" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+body-set-collision-mask :class 'physics-server-3d :bind
  "body_set_collision_mask" :hash 3411492887)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-3d+body-get-collision-mask :class 'physics-server-3d :bind
  "body_get_collision_mask" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+body-set-collision-priority :class 'physics-server-3d :bind
  "body_set_collision_priority" :hash 1794382983)
 :void (body rid) (priority float))

(defgmethod
 (physics-server-3d+body-get-collision-priority :class 'physics-server-3d :bind
  "body_get_collision_priority" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+body-add-shape :class 'physics-server-3d :bind
  "body_add_shape" :hash 3711419014)
 :void (body rid) (shape rid) (transform transform-3d) (disabled bool))

(defgmethod
 (physics-server-3d+body-set-shape :class 'physics-server-3d :bind
  "body_set_shape" :hash 2310537182)
 :void (body rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-3d+body-set-shape-transform :class 'physics-server-3d :bind
  "body_set_shape_transform" :hash 675327471)
 :void (body rid) (shape-idx int) (transform transform-3d))

(defgmethod
 (physics-server-3d+body-set-shape-disabled :class 'physics-server-3d :bind
  "body_set_shape_disabled" :hash 2658558584)
 :void (body rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-3d+body-get-shape-count :class 'physics-server-3d :bind
  "body_get_shape_count" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+body-get-shape :class 'physics-server-3d :bind
  "body_get_shape" :hash 1066463050)
 rid (body rid) (shape-idx int))

(defgmethod
 (physics-server-3d+body-get-shape-transform :class 'physics-server-3d :bind
  "body_get_shape_transform" :hash 1050775521)
 transform-3d (body rid) (shape-idx int))

(defgmethod
 (physics-server-3d+body-remove-shape :class 'physics-server-3d :bind
  "body_remove_shape" :hash 3411492887)
 :void (body rid) (shape-idx int))

(defgmethod
 (physics-server-3d+body-clear-shapes :class 'physics-server-3d :bind
  "body_clear_shapes" :hash 2722037293)
 :void (body rid))

(defgmethod
 (physics-server-3d+body-attach-object-instance-id :class 'physics-server-3d
  :bind "body_attach_object_instance_id" :hash 3411492887)
 :void (body rid) (id int))

(defgmethod
 (physics-server-3d+body-get-object-instance-id :class 'physics-server-3d :bind
  "body_get_object_instance_id" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+body-set-enable-continuous-collision-detection :class
  'physics-server-3d :bind "body_set_enable_continuous_collision_detection"
  :hash 1265174801)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3d+body-is-continuous-collision-detection-enabled :class
  'physics-server-3d :bind "body_is_continuous_collision_detection_enabled"
  :hash 4155700596)
 bool (body rid))

(defgmethod
 (physics-server-3d+body-set-param :class 'physics-server-3d :bind
  "body_set_param" :hash 910941953)
 :void (body rid) (param physics-server-3d+body-parameter) (value variant))

(defgmethod
 (physics-server-3d+body-get-param :class 'physics-server-3d :bind
  "body_get_param" :hash 3385027841)
 variant (body rid) (param physics-server-3d+body-parameter))

(defgmethod
 (physics-server-3d+body-reset-mass-properties :class 'physics-server-3d :bind
  "body_reset_mass_properties" :hash 2722037293)
 :void (body rid))

(defgmethod
 (physics-server-3d+body-set-state :class 'physics-server-3d :bind
  "body_set_state" :hash 599977762)
 :void (body rid) (state physics-server-3d+body-state) (value variant))

(defgmethod
 (physics-server-3d+body-get-state :class 'physics-server-3d :bind
  "body_get_state" :hash 1850449534)
 variant (body rid) (state physics-server-3d+body-state))

(defgmethod
 (physics-server-3d+body-apply-central-impulse :class 'physics-server-3d :bind
  "body_apply_central_impulse" :hash 3227306858)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3d+body-apply-impulse :class 'physics-server-3d :bind
  "body_apply_impulse" :hash 390416203)
 :void (body rid) (impulse vector-3) (position vector-3))

(defgmethod
 (physics-server-3d+body-apply-torque-impulse :class 'physics-server-3d :bind
  "body_apply_torque_impulse" :hash 3227306858)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3d+body-apply-central-force :class 'physics-server-3d :bind
  "body_apply_central_force" :hash 3227306858)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3d+body-apply-force :class 'physics-server-3d :bind
  "body_apply_force" :hash 390416203)
 :void (body rid) (force vector-3) (position vector-3))

(defgmethod
 (physics-server-3d+body-apply-torque :class 'physics-server-3d :bind
  "body_apply_torque" :hash 3227306858)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3d+body-add-constant-central-force :class 'physics-server-3d
  :bind "body_add_constant_central_force" :hash 3227306858)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3d+body-add-constant-force :class 'physics-server-3d :bind
  "body_add_constant_force" :hash 390416203)
 :void (body rid) (force vector-3) (position vector-3))

(defgmethod
 (physics-server-3d+body-add-constant-torque :class 'physics-server-3d :bind
  "body_add_constant_torque" :hash 3227306858)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3d+body-set-constant-force :class 'physics-server-3d :bind
  "body_set_constant_force" :hash 3227306858)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3d+body-get-constant-force :class 'physics-server-3d :bind
  "body_get_constant_force" :hash 531438156)
 vector-3 (body rid))

(defgmethod
 (physics-server-3d+body-set-constant-torque :class 'physics-server-3d :bind
  "body_set_constant_torque" :hash 3227306858)
 :void (body rid) (torque vector-3))

(defgmethod
 (physics-server-3d+body-get-constant-torque :class 'physics-server-3d :bind
  "body_get_constant_torque" :hash 531438156)
 vector-3 (body rid))

(defgmethod
 (physics-server-3d+body-set-axis-velocity :class 'physics-server-3d :bind
  "body_set_axis_velocity" :hash 3227306858)
 :void (body rid) (axis-velocity vector-3))

(defgmethod
 (physics-server-3d+body-set-axis-lock :class 'physics-server-3d :bind
  "body_set_axis_lock" :hash 2020836892)
 :void (body rid) (axis physics-server-3d+body-axis) (lock bool))

(defgmethod
 (physics-server-3d+body-is-axis-locked :class 'physics-server-3d :bind
  "body_is_axis_locked" :hash 587853580)
 bool (body rid) (axis physics-server-3d+body-axis))

(defgmethod
 (physics-server-3d+body-add-collision-exception :class 'physics-server-3d
  :bind "body_add_collision_exception" :hash 395945892)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-3d+body-remove-collision-exception :class 'physics-server-3d
  :bind "body_remove_collision_exception" :hash 395945892)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-3d+body-set-max-contacts-reported :class 'physics-server-3d
  :bind "body_set_max_contacts_reported" :hash 3411492887)
 :void (body rid) (amount int))

(defgmethod
 (physics-server-3d+body-get-max-contacts-reported :class 'physics-server-3d
  :bind "body_get_max_contacts_reported" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+body-set-omit-force-integration :class 'physics-server-3d
  :bind "body_set_omit_force_integration" :hash 1265174801)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3d+body-is-omitting-force-integration :class
  'physics-server-3d :bind "body_is_omitting_force_integration" :hash
  4155700596)
 bool (body rid))

(defgmethod
 (physics-server-3d+body-set-state-sync-callback :class 'physics-server-3d
  :bind "body_set_state_sync_callback" :hash 3379118538)
 :void (body rid) (callable callable))

(defgmethod
 (physics-server-3d+body-set-force-integration-callback :class
  'physics-server-3d :bind "body_set_force_integration_callback" :hash
  3059434249)
 :void (body rid) (callable callable) (userdata variant))

(defgmethod
 (physics-server-3d+body-set-ray-pickable :class 'physics-server-3d :bind
  "body_set_ray_pickable" :hash 1265174801)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3d+body-test-motion :class 'physics-server-3d :bind
  "body_test_motion" :hash 1944921792)
 bool (body rid) (parameters physics-test-motion-parameters-3d)
 (result physics-test-motion-result-3d))

(defgmethod
 (physics-server-3d+body-get-direct-state :class 'physics-server-3d :bind
  "body_get_direct_state" :hash 3029727957)
 physics-direct-body-state-3d (body rid))

(defgmethod
 (physics-server-3d+soft-body-create :class 'physics-server-3d :bind
  "soft_body_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+soft-body-update-rendering-server :class 'physics-server-3d
  :bind "soft_body_update_rendering_server" :hash 2218179753)
 :void (body rid)
 (rendering-server-handler physics-server-3drendering-server-handler))

(defgmethod
 (physics-server-3d+soft-body-set-space :class 'physics-server-3d :bind
  "soft_body_set_space" :hash 395945892)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-3d+soft-body-get-space :class 'physics-server-3d :bind
  "soft_body_get_space" :hash 3814569979)
 rid (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-mesh :class 'physics-server-3d :bind
  "soft_body_set_mesh" :hash 395945892)
 :void (body rid) (mesh rid))

(defgmethod
 (physics-server-3d+soft-body-get-bounds :class 'physics-server-3d :bind
  "soft_body_get_bounds" :hash 974181306)
 aabb (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-collision-layer :class 'physics-server-3d
  :bind "soft_body_set_collision_layer" :hash 3411492887)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-3d+soft-body-get-collision-layer :class 'physics-server-3d
  :bind "soft_body_get_collision_layer" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-collision-mask :class 'physics-server-3d
  :bind "soft_body_set_collision_mask" :hash 3411492887)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-3d+soft-body-get-collision-mask :class 'physics-server-3d
  :bind "soft_body_get_collision_mask" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+soft-body-add-collision-exception :class 'physics-server-3d
  :bind "soft_body_add_collision_exception" :hash 395945892)
 :void (body rid) (body-b rid))

(defgmethod
 (physics-server-3d+soft-body-remove-collision-exception :class
  'physics-server-3d :bind "soft_body_remove_collision_exception" :hash
  395945892)
 :void (body rid) (body-b rid))

(defgmethod
 (physics-server-3d+soft-body-set-state :class 'physics-server-3d :bind
  "soft_body_set_state" :hash 599977762)
 :void (body rid) (state physics-server-3d+body-state) (variant variant))

(defgmethod
 (physics-server-3d+soft-body-get-state :class 'physics-server-3d :bind
  "soft_body_get_state" :hash 1850449534)
 variant (body rid) (state physics-server-3d+body-state))

(defgmethod
 (physics-server-3d+soft-body-set-transform :class 'physics-server-3d :bind
  "soft_body_set_transform" :hash 3935195649)
 :void (body rid) (transform transform-3d))

(defgmethod
 (physics-server-3d+soft-body-set-ray-pickable :class 'physics-server-3d :bind
  "soft_body_set_ray_pickable" :hash 1265174801)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-3d+soft-body-set-simulation-precision :class
  'physics-server-3d :bind "soft_body_set_simulation_precision" :hash
  3411492887)
 :void (body rid) (simulation-precision int))

(defgmethod
 (physics-server-3d+soft-body-get-simulation-precision :class
  'physics-server-3d :bind "soft_body_get_simulation_precision" :hash
  2198884583)
 int (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-total-mass :class 'physics-server-3d :bind
  "soft_body_set_total_mass" :hash 1794382983)
 :void (body rid) (total-mass float))

(defgmethod
 (physics-server-3d+soft-body-get-total-mass :class 'physics-server-3d :bind
  "soft_body_get_total_mass" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-linear-stiffness :class 'physics-server-3d
  :bind "soft_body_set_linear_stiffness" :hash 1794382983)
 :void (body rid) (stiffness float))

(defgmethod
 (physics-server-3d+soft-body-get-linear-stiffness :class 'physics-server-3d
  :bind "soft_body_get_linear_stiffness" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-shrinking-factor :class 'physics-server-3d
  :bind "soft_body_set_shrinking_factor" :hash 1794382983)
 :void (body rid) (shrinking-factor float))

(defgmethod
 (physics-server-3d+soft-body-get-shrinking-factor :class 'physics-server-3d
  :bind "soft_body_get_shrinking_factor" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-pressure-coefficient :class
  'physics-server-3d :bind "soft_body_set_pressure_coefficient" :hash
  1794382983)
 :void (body rid) (pressure-coefficient float))

(defgmethod
 (physics-server-3d+soft-body-get-pressure-coefficient :class
  'physics-server-3d :bind "soft_body_get_pressure_coefficient" :hash
  866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-damping-coefficient :class 'physics-server-3d
  :bind "soft_body_set_damping_coefficient" :hash 1794382983)
 :void (body rid) (damping-coefficient float))

(defgmethod
 (physics-server-3d+soft-body-get-damping-coefficient :class 'physics-server-3d
  :bind "soft_body_get_damping_coefficient" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-set-drag-coefficient :class 'physics-server-3d
  :bind "soft_body_set_drag_coefficient" :hash 1794382983)
 :void (body rid) (drag-coefficient float))

(defgmethod
 (physics-server-3d+soft-body-get-drag-coefficient :class 'physics-server-3d
  :bind "soft_body_get_drag_coefficient" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-3d+soft-body-move-point :class 'physics-server-3d :bind
  "soft_body_move_point" :hash 831953689)
 :void (body rid) (point-index int) (global-position vector-3))

(defgmethod
 (physics-server-3d+soft-body-get-point-global-position :class
  'physics-server-3d :bind "soft_body_get_point_global_position" :hash
  3440143363)
 vector-3 (body rid) (point-index int))

(defgmethod
 (physics-server-3d+soft-body-remove-all-pinned-points :class
  'physics-server-3d :bind "soft_body_remove_all_pinned_points" :hash
  2722037293)
 :void (body rid))

(defgmethod
 (physics-server-3d+soft-body-pin-point :class 'physics-server-3d :bind
  "soft_body_pin_point" :hash 2658558584)
 :void (body rid) (point-index int) (pin bool))

(defgmethod
 (physics-server-3d+soft-body-is-point-pinned :class 'physics-server-3d :bind
  "soft_body_is_point_pinned" :hash 3120086654)
 bool (body rid) (point-index int))

(defgmethod
 (physics-server-3d+soft-body-apply-point-impulse :class 'physics-server-3d
  :bind "soft_body_apply_point_impulse" :hash 831953689)
 :void (body rid) (point-index int) (impulse vector-3))

(defgmethod
 (physics-server-3d+soft-body-apply-point-force :class 'physics-server-3d :bind
  "soft_body_apply_point_force" :hash 831953689)
 :void (body rid) (point-index int) (force vector-3))

(defgmethod
 (physics-server-3d+soft-body-apply-central-impulse :class 'physics-server-3d
  :bind "soft_body_apply_central_impulse" :hash 3227306858)
 :void (body rid) (impulse vector-3))

(defgmethod
 (physics-server-3d+soft-body-apply-central-force :class 'physics-server-3d
  :bind "soft_body_apply_central_force" :hash 3227306858)
 :void (body rid) (force vector-3))

(defgmethod
 (physics-server-3d+joint-create :class 'physics-server-3d :bind "joint_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-3d+joint-clear :class 'physics-server-3d :bind "joint_clear"
  :hash 2722037293)
 :void (joint rid))

(defgmethod
 (physics-server-3d+joint-make-pin :class 'physics-server-3d :bind
  "joint_make_pin" :hash 4280171926)
 :void (joint rid) (body-a rid) (local-a vector-3) (body-b rid)
 (local-b vector-3))

(defgmethod
 (physics-server-3d+pin-joint-set-param :class 'physics-server-3d :bind
  "pin_joint_set_param" :hash 810685294)
 :void (joint rid) (param physics-server-3d+pin-joint-param) (value float))

(defgmethod
 (physics-server-3d+pin-joint-get-param :class 'physics-server-3d :bind
  "pin_joint_get_param" :hash 2817972347)
 float (joint rid) (param physics-server-3d+pin-joint-param))

(defgmethod
 (physics-server-3d+pin-joint-set-local-a :class 'physics-server-3d :bind
  "pin_joint_set_local_a" :hash 3227306858)
 :void (joint rid) (local-a vector-3))

(defgmethod
 (physics-server-3d+pin-joint-get-local-a :class 'physics-server-3d :bind
  "pin_joint_get_local_a" :hash 531438156)
 vector-3 (joint rid))

(defgmethod
 (physics-server-3d+pin-joint-set-local-b :class 'physics-server-3d :bind
  "pin_joint_set_local_b" :hash 3227306858)
 :void (joint rid) (local-b vector-3))

(defgmethod
 (physics-server-3d+pin-joint-get-local-b :class 'physics-server-3d :bind
  "pin_joint_get_local_b" :hash 531438156)
 vector-3 (joint rid))

(defgmethod
 (physics-server-3d+joint-make-hinge :class 'physics-server-3d :bind
  "joint_make_hinge" :hash 1684107643)
 :void (joint rid) (body-a rid) (hinge-a transform-3d) (body-b rid)
 (hinge-b transform-3d))

(defgmethod
 (physics-server-3d+hinge-joint-set-param :class 'physics-server-3d :bind
  "hinge_joint_set_param" :hash 3165502333)
 :void (joint rid) (param physics-server-3d+hinge-joint-param) (value float))

(defgmethod
 (physics-server-3d+hinge-joint-get-param :class 'physics-server-3d :bind
  "hinge_joint_get_param" :hash 2129207581)
 float (joint rid) (param physics-server-3d+hinge-joint-param))

(defgmethod
 (physics-server-3d+hinge-joint-set-flag :class 'physics-server-3d :bind
  "hinge_joint_set_flag" :hash 1601626188)
 :void (joint rid) (flag physics-server-3d+hinge-joint-flag) (enabled bool))

(defgmethod
 (physics-server-3d+hinge-joint-get-flag :class 'physics-server-3d :bind
  "hinge_joint_get_flag" :hash 4165147865)
 bool (joint rid) (flag physics-server-3d+hinge-joint-flag))

(defgmethod
 (physics-server-3d+joint-make-slider :class 'physics-server-3d :bind
  "joint_make_slider" :hash 1684107643)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3d+slider-joint-set-param :class 'physics-server-3d :bind
  "slider_joint_set_param" :hash 2264833593)
 :void (joint rid) (param physics-server-3d+slider-joint-param) (value float))

(defgmethod
 (physics-server-3d+slider-joint-get-param :class 'physics-server-3d :bind
  "slider_joint_get_param" :hash 3498644957)
 float (joint rid) (param physics-server-3d+slider-joint-param))

(defgmethod
 (physics-server-3d+joint-make-cone-twist :class 'physics-server-3d :bind
  "joint_make_cone_twist" :hash 1684107643)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3d+cone-twist-joint-set-param :class 'physics-server-3d :bind
  "cone_twist_joint_set_param" :hash 808587618)
 :void (joint rid) (param physics-server-3d+cone-twist-joint-param)
 (value float))

(defgmethod
 (physics-server-3d+cone-twist-joint-get-param :class 'physics-server-3d :bind
  "cone_twist_joint_get_param" :hash 1134789658)
 float (joint rid) (param physics-server-3d+cone-twist-joint-param))

(defgmethod
 (physics-server-3d+joint-get-type :class 'physics-server-3d :bind
  "joint_get_type" :hash 4290791900)
 physics-server-3d+joint-type (joint rid))

(defgmethod
 (physics-server-3d+joint-set-solver-priority :class 'physics-server-3d :bind
  "joint_set_solver_priority" :hash 3411492887)
 :void (joint rid) (priority int))

(defgmethod
 (physics-server-3d+joint-get-solver-priority :class 'physics-server-3d :bind
  "joint_get_solver_priority" :hash 2198884583)
 int (joint rid))

(defgmethod
 (physics-server-3d+joint-disable-collisions-between-bodies :class
  'physics-server-3d :bind "joint_disable_collisions_between_bodies" :hash
  1265174801)
 :void (joint rid) (disable bool))

(defgmethod
 (physics-server-3d+joint-is-disabled-collisions-between-bodies :class
  'physics-server-3d :bind "joint_is_disabled_collisions_between_bodies" :hash
  4155700596)
 bool (joint rid))

(defgmethod
 (physics-server-3d+joint-make-generic-6dof :class 'physics-server-3d :bind
  "joint_make_generic_6dof" :hash 1684107643)
 :void (joint rid) (body-a rid) (local-ref-a transform-3d) (body-b rid)
 (local-ref-b transform-3d))

(defgmethod
 (physics-server-3d+generic-6dof-joint-set-param :class 'physics-server-3d
  :bind "generic_6dof_joint_set_param" :hash 2600081391)
 :void (joint rid) (axis vector-3+axis)
 (param physics-server-3d+g6dofjoint-axis-param) (value float))

(defgmethod
 (physics-server-3d+generic-6dof-joint-get-param :class 'physics-server-3d
  :bind "generic_6dof_joint_get_param" :hash 467122058)
 float (joint rid) (axis vector-3+axis)
 (param physics-server-3d+g6dofjoint-axis-param))

(defgmethod
 (physics-server-3d+generic-6dof-joint-set-flag :class 'physics-server-3d :bind
  "generic_6dof_joint_set_flag" :hash 3570926903)
 :void (joint rid) (axis vector-3+axis)
 (flag physics-server-3d+g6dofjoint-axis-flag) (enable bool))

(defgmethod
 (physics-server-3d+generic-6dof-joint-get-flag :class 'physics-server-3d :bind
  "generic_6dof_joint_get_flag" :hash 4158090196)
 bool (joint rid) (axis vector-3+axis)
 (flag physics-server-3d+g6dofjoint-axis-flag))

(defgmethod
 (physics-server-3d+free-rid :class 'physics-server-3d :bind "free_rid" :hash
  2722037293)
 :void (rid rid))

(defgmethod
 (physics-server-3d+set-active :class 'physics-server-3d :bind "set_active"
  :hash 2586408642)
 :void (active bool))

(defgmethod
 (physics-server-3d+get-process-info :class 'physics-server-3d :bind
  "get_process_info" :hash 1332958745)
 int (process-info physics-server-3d+process-info))