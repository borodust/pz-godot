(common-lisp:in-package :%godot)


(defgmethod
 (physics-server-2d+world-boundary-shape-create :class 'physics-server-2d :bind
  "world_boundary_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+separation-ray-shape-create :class 'physics-server-2d :bind
  "separation_ray_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+segment-shape-create :class 'physics-server-2d :bind
  "segment_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+circle-shape-create :class 'physics-server-2d :bind
  "circle_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+rectangle-shape-create :class 'physics-server-2d :bind
  "rectangle_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+capsule-shape-create :class 'physics-server-2d :bind
  "capsule_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+convex-polygon-shape-create :class 'physics-server-2d :bind
  "convex_polygon_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+concave-polygon-shape-create :class 'physics-server-2d
  :bind "concave_polygon_shape_create" :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+shape-set-data :class 'physics-server-2d :bind
  "shape_set_data" :hash 3175752987)
 :void (shape rid) (data variant))

(defgmethod
 (physics-server-2d+shape-get-type :class 'physics-server-2d :bind
  "shape_get_type" :hash 1240598777)
 physics-server-2d+shape-type (shape rid))

(defgmethod
 (physics-server-2d+shape-get-data :class 'physics-server-2d :bind
  "shape_get_data" :hash 4171304767)
 variant (shape rid))

(defgmethod
 (physics-server-2d+space-create :class 'physics-server-2d :bind "space_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+space-set-active :class 'physics-server-2d :bind
  "space_set_active" :hash 1265174801)
 :void (space rid) (active bool))

(defgmethod
 (physics-server-2d+space-is-active :class 'physics-server-2d :bind
  "space_is_active" :hash 4155700596)
 bool (space rid))

(defgmethod
 (physics-server-2d+space-set-param :class 'physics-server-2d :bind
  "space_set_param" :hash 949194586)
 :void (space rid) (param physics-server-2d+space-parameter) (value float))

(defgmethod
 (physics-server-2d+space-get-param :class 'physics-server-2d :bind
  "space_get_param" :hash 874111783)
 float (space rid) (param physics-server-2d+space-parameter))

(defgmethod
 (physics-server-2d+space-get-direct-state :class 'physics-server-2d :bind
  "space_get_direct_state" :hash 3160173886)
 physics-direct-space-state-2d (space rid))

(defgmethod
 (physics-server-2d+area-create :class 'physics-server-2d :bind "area_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+area-set-space :class 'physics-server-2d :bind
  "area_set_space" :hash 395945892)
 :void (area rid) (space rid))

(defgmethod
 (physics-server-2d+area-get-space :class 'physics-server-2d :bind
  "area_get_space" :hash 3814569979)
 rid (area rid))

(defgmethod
 (physics-server-2d+area-add-shape :class 'physics-server-2d :bind
  "area_add_shape" :hash 339056240)
 :void (area rid) (shape rid) (transform transform-2d) (disabled bool))

(defgmethod
 (physics-server-2d+area-set-shape :class 'physics-server-2d :bind
  "area_set_shape" :hash 2310537182)
 :void (area rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-2d+area-set-shape-transform :class 'physics-server-2d :bind
  "area_set_shape_transform" :hash 736082694)
 :void (area rid) (shape-idx int) (transform transform-2d))

(defgmethod
 (physics-server-2d+area-set-shape-disabled :class 'physics-server-2d :bind
  "area_set_shape_disabled" :hash 2658558584)
 :void (area rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-2d+area-get-shape-count :class 'physics-server-2d :bind
  "area_get_shape_count" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-2d+area-get-shape :class 'physics-server-2d :bind
  "area_get_shape" :hash 1066463050)
 rid (area rid) (shape-idx int))

(defgmethod
 (physics-server-2d+area-get-shape-transform :class 'physics-server-2d :bind
  "area_get_shape_transform" :hash 1324854622)
 transform-2d (area rid) (shape-idx int))

(defgmethod
 (physics-server-2d+area-remove-shape :class 'physics-server-2d :bind
  "area_remove_shape" :hash 3411492887)
 :void (area rid) (shape-idx int))

(defgmethod
 (physics-server-2d+area-clear-shapes :class 'physics-server-2d :bind
  "area_clear_shapes" :hash 2722037293)
 :void (area rid))

(defgmethod
 (physics-server-2d+area-set-collision-layer :class 'physics-server-2d :bind
  "area_set_collision_layer" :hash 3411492887)
 :void (area rid) (layer int))

(defgmethod
 (physics-server-2d+area-get-collision-layer :class 'physics-server-2d :bind
  "area_get_collision_layer" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-2d+area-set-collision-mask :class 'physics-server-2d :bind
  "area_set_collision_mask" :hash 3411492887)
 :void (area rid) (mask int))

(defgmethod
 (physics-server-2d+area-get-collision-mask :class 'physics-server-2d :bind
  "area_get_collision_mask" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-2d+area-set-param :class 'physics-server-2d :bind
  "area_set_param" :hash 1257146028)
 :void (area rid) (param physics-server-2d+area-parameter) (value variant))

(defgmethod
 (physics-server-2d+area-set-transform :class 'physics-server-2d :bind
  "area_set_transform" :hash 1246044741)
 :void (area rid) (transform transform-2d))

(defgmethod
 (physics-server-2d+area-get-param :class 'physics-server-2d :bind
  "area_get_param" :hash 3047435120)
 variant (area rid) (param physics-server-2d+area-parameter))

(defgmethod
 (physics-server-2d+area-get-transform :class 'physics-server-2d :bind
  "area_get_transform" :hash 213527486)
 transform-2d (area rid))

(defgmethod
 (physics-server-2d+area-attach-object-instance-id :class 'physics-server-2d
  :bind "area_attach_object_instance_id" :hash 3411492887)
 :void (area rid) (id int))

(defgmethod
 (physics-server-2d+area-get-object-instance-id :class 'physics-server-2d :bind
  "area_get_object_instance_id" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-2d+area-attach-canvas-instance-id :class 'physics-server-2d
  :bind "area_attach_canvas_instance_id" :hash 3411492887)
 :void (area rid) (id int))

(defgmethod
 (physics-server-2d+area-get-canvas-instance-id :class 'physics-server-2d :bind
  "area_get_canvas_instance_id" :hash 2198884583)
 int (area rid))

(defgmethod
 (physics-server-2d+area-set-monitor-callback :class 'physics-server-2d :bind
  "area_set_monitor_callback" :hash 3379118538)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-2d+area-set-area-monitor-callback :class 'physics-server-2d
  :bind "area_set_area_monitor_callback" :hash 3379118538)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-2d+area-set-monitorable :class 'physics-server-2d :bind
  "area_set_monitorable" :hash 1265174801)
 :void (area rid) (monitorable bool))

(defgmethod
 (physics-server-2d+body-create :class 'physics-server-2d :bind "body_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+body-set-space :class 'physics-server-2d :bind
  "body_set_space" :hash 395945892)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-2d+body-get-space :class 'physics-server-2d :bind
  "body_get_space" :hash 3814569979)
 rid (body rid))

(defgmethod
 (physics-server-2d+body-set-mode :class 'physics-server-2d :bind
  "body_set_mode" :hash 1658067650)
 :void (body rid) (mode physics-server-2d+body-mode))

(defgmethod
 (physics-server-2d+body-get-mode :class 'physics-server-2d :bind
  "body_get_mode" :hash 3261702585)
 physics-server-2d+body-mode (body rid))

(defgmethod
 (physics-server-2d+body-add-shape :class 'physics-server-2d :bind
  "body_add_shape" :hash 339056240)
 :void (body rid) (shape rid) (transform transform-2d) (disabled bool))

(defgmethod
 (physics-server-2d+body-set-shape :class 'physics-server-2d :bind
  "body_set_shape" :hash 2310537182)
 :void (body rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-2d+body-set-shape-transform :class 'physics-server-2d :bind
  "body_set_shape_transform" :hash 736082694)
 :void (body rid) (shape-idx int) (transform transform-2d))

(defgmethod
 (physics-server-2d+body-get-shape-count :class 'physics-server-2d :bind
  "body_get_shape_count" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-get-shape :class 'physics-server-2d :bind
  "body_get_shape" :hash 1066463050)
 rid (body rid) (shape-idx int))

(defgmethod
 (physics-server-2d+body-get-shape-transform :class 'physics-server-2d :bind
  "body_get_shape_transform" :hash 1324854622)
 transform-2d (body rid) (shape-idx int))

(defgmethod
 (physics-server-2d+body-remove-shape :class 'physics-server-2d :bind
  "body_remove_shape" :hash 3411492887)
 :void (body rid) (shape-idx int))

(defgmethod
 (physics-server-2d+body-clear-shapes :class 'physics-server-2d :bind
  "body_clear_shapes" :hash 2722037293)
 :void (body rid))

(defgmethod
 (physics-server-2d+body-set-shape-disabled :class 'physics-server-2d :bind
  "body_set_shape_disabled" :hash 2658558584)
 :void (body rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-2d+body-set-shape-as-one-way-collision :class
  'physics-server-2d :bind "body_set_shape_as_one_way_collision" :hash
  2389283141)
 :void (body rid) (shape-idx int) (enable bool) (margin float)
 (direction vector-2))

(defgmethod
 (physics-server-2d+body-attach-object-instance-id :class 'physics-server-2d
  :bind "body_attach_object_instance_id" :hash 3411492887)
 :void (body rid) (id int))

(defgmethod
 (physics-server-2d+body-get-object-instance-id :class 'physics-server-2d :bind
  "body_get_object_instance_id" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-attach-canvas-instance-id :class 'physics-server-2d
  :bind "body_attach_canvas_instance_id" :hash 3411492887)
 :void (body rid) (id int))

(defgmethod
 (physics-server-2d+body-get-canvas-instance-id :class 'physics-server-2d :bind
  "body_get_canvas_instance_id" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-set-continuous-collision-detection-mode :class
  'physics-server-2d :bind "body_set_continuous_collision_detection_mode" :hash
  1882257015)
 :void (body rid) (mode physics-server-2d+ccdmode))

(defgmethod
 (physics-server-2d+body-get-continuous-collision-detection-mode :class
  'physics-server-2d :bind "body_get_continuous_collision_detection_mode" :hash
  2661282217)
 physics-server-2d+ccdmode (body rid))

(defgmethod
 (physics-server-2d+body-set-collision-layer :class 'physics-server-2d :bind
  "body_set_collision_layer" :hash 3411492887)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-2d+body-get-collision-layer :class 'physics-server-2d :bind
  "body_get_collision_layer" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-set-collision-mask :class 'physics-server-2d :bind
  "body_set_collision_mask" :hash 3411492887)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-2d+body-get-collision-mask :class 'physics-server-2d :bind
  "body_get_collision_mask" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-set-collision-priority :class 'physics-server-2d :bind
  "body_set_collision_priority" :hash 1794382983)
 :void (body rid) (priority float))

(defgmethod
 (physics-server-2d+body-get-collision-priority :class 'physics-server-2d :bind
  "body_get_collision_priority" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-2d+body-set-param :class 'physics-server-2d :bind
  "body_set_param" :hash 2715630609)
 :void (body rid) (param physics-server-2d+body-parameter) (value variant))

(defgmethod
 (physics-server-2d+body-get-param :class 'physics-server-2d :bind
  "body_get_param" :hash 3208033526)
 variant (body rid) (param physics-server-2d+body-parameter))

(defgmethod
 (physics-server-2d+body-reset-mass-properties :class 'physics-server-2d :bind
  "body_reset_mass_properties" :hash 2722037293)
 :void (body rid))

(defgmethod
 (physics-server-2d+body-set-state :class 'physics-server-2d :bind
  "body_set_state" :hash 1706355209)
 :void (body rid) (state physics-server-2d+body-state) (value variant))

(defgmethod
 (physics-server-2d+body-get-state :class 'physics-server-2d :bind
  "body_get_state" :hash 4036367961)
 variant (body rid) (state physics-server-2d+body-state))

(defgmethod
 (physics-server-2d+body-apply-central-impulse :class 'physics-server-2d :bind
  "body_apply_central_impulse" :hash 3201125042)
 :void (body rid) (impulse vector-2))

(defgmethod
 (physics-server-2d+body-apply-torque-impulse :class 'physics-server-2d :bind
  "body_apply_torque_impulse" :hash 1794382983)
 :void (body rid) (impulse float))

(defgmethod
 (physics-server-2d+body-apply-impulse :class 'physics-server-2d :bind
  "body_apply_impulse" :hash 205485391)
 :void (body rid) (impulse vector-2) (position vector-2))

(defgmethod
 (physics-server-2d+body-apply-central-force :class 'physics-server-2d :bind
  "body_apply_central_force" :hash 3201125042)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2d+body-apply-force :class 'physics-server-2d :bind
  "body_apply_force" :hash 205485391)
 :void (body rid) (force vector-2) (position vector-2))

(defgmethod
 (physics-server-2d+body-apply-torque :class 'physics-server-2d :bind
  "body_apply_torque" :hash 1794382983)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2d+body-add-constant-central-force :class 'physics-server-2d
  :bind "body_add_constant_central_force" :hash 3201125042)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2d+body-add-constant-force :class 'physics-server-2d :bind
  "body_add_constant_force" :hash 205485391)
 :void (body rid) (force vector-2) (position vector-2))

(defgmethod
 (physics-server-2d+body-add-constant-torque :class 'physics-server-2d :bind
  "body_add_constant_torque" :hash 1794382983)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2d+body-set-constant-force :class 'physics-server-2d :bind
  "body_set_constant_force" :hash 3201125042)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2d+body-get-constant-force :class 'physics-server-2d :bind
  "body_get_constant_force" :hash 2440833711)
 vector-2 (body rid))

(defgmethod
 (physics-server-2d+body-set-constant-torque :class 'physics-server-2d :bind
  "body_set_constant_torque" :hash 1794382983)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2d+body-get-constant-torque :class 'physics-server-2d :bind
  "body_get_constant_torque" :hash 866169185)
 float (body rid))

(defgmethod
 (physics-server-2d+body-set-axis-velocity :class 'physics-server-2d :bind
  "body_set_axis_velocity" :hash 3201125042)
 :void (body rid) (axis-velocity vector-2))

(defgmethod
 (physics-server-2d+body-add-collision-exception :class 'physics-server-2d
  :bind "body_add_collision_exception" :hash 395945892)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-2d+body-remove-collision-exception :class 'physics-server-2d
  :bind "body_remove_collision_exception" :hash 395945892)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-2d+body-set-max-contacts-reported :class 'physics-server-2d
  :bind "body_set_max_contacts_reported" :hash 3411492887)
 :void (body rid) (amount int))

(defgmethod
 (physics-server-2d+body-get-max-contacts-reported :class 'physics-server-2d
  :bind "body_get_max_contacts_reported" :hash 2198884583)
 int (body rid))

(defgmethod
 (physics-server-2d+body-set-omit-force-integration :class 'physics-server-2d
  :bind "body_set_omit_force_integration" :hash 1265174801)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-2d+body-is-omitting-force-integration :class
  'physics-server-2d :bind "body_is_omitting_force_integration" :hash
  4155700596)
 bool (body rid))

(defgmethod
 (physics-server-2d+body-set-state-sync-callback :class 'physics-server-2d
  :bind "body_set_state_sync_callback" :hash 3379118538)
 :void (body rid) (callable callable))

(defgmethod
 (physics-server-2d+body-set-force-integration-callback :class
  'physics-server-2d :bind "body_set_force_integration_callback" :hash
  3059434249)
 :void (body rid) (callable callable) (userdata variant))

(defgmethod
 (physics-server-2d+body-test-motion :class 'physics-server-2d :bind
  "body_test_motion" :hash 1699844009)
 bool (body rid) (parameters physics-test-motion-parameters-2d)
 (result physics-test-motion-result-2d))

(defgmethod
 (physics-server-2d+body-get-direct-state :class 'physics-server-2d :bind
  "body_get_direct_state" :hash 1191931871)
 physics-direct-body-state-2d (body rid))

(defgmethod
 (physics-server-2d+joint-create :class 'physics-server-2d :bind "joint_create"
  :hash 529393457)
 rid)

(defgmethod
 (physics-server-2d+joint-clear :class 'physics-server-2d :bind "joint_clear"
  :hash 2722037293)
 :void (joint rid))

(defgmethod
 (physics-server-2d+joint-set-param :class 'physics-server-2d :bind
  "joint_set_param" :hash 3972556514)
 :void (joint rid) (param physics-server-2d+joint-param) (value float))

(defgmethod
 (physics-server-2d+joint-get-param :class 'physics-server-2d :bind
  "joint_get_param" :hash 4016448949)
 float (joint rid) (param physics-server-2d+joint-param))

(defgmethod
 (physics-server-2d+joint-disable-collisions-between-bodies :class
  'physics-server-2d :bind "joint_disable_collisions_between_bodies" :hash
  1265174801)
 :void (joint rid) (disable bool))

(defgmethod
 (physics-server-2d+joint-is-disabled-collisions-between-bodies :class
  'physics-server-2d :bind "joint_is_disabled_collisions_between_bodies" :hash
  4155700596)
 bool (joint rid))

(defgmethod
 (physics-server-2d+joint-make-pin :class 'physics-server-2d :bind
  "joint_make_pin" :hash 1612646186)
 :void (joint rid) (anchor vector-2) (body-a rid) (body-b rid))

(defgmethod
 (physics-server-2d+joint-make-groove :class 'physics-server-2d :bind
  "joint_make_groove" :hash 481430435)
 :void (joint rid) (groove1-a vector-2) (groove2-a vector-2)
 (anchor-b vector-2) (body-a rid) (body-b rid))

(defgmethod
 (physics-server-2d+joint-make-damped-spring :class 'physics-server-2d :bind
  "joint_make_damped_spring" :hash 1994657646)
 :void (joint rid) (anchor-a vector-2) (anchor-b vector-2) (body-a rid)
 (body-b rid))

(defgmethod
 (physics-server-2d+pin-joint-set-flag :class 'physics-server-2d :bind
  "pin_joint_set_flag" :hash 3520002352)
 :void (joint rid) (flag physics-server-2d+pin-joint-flag) (enabled bool))

(defgmethod
 (physics-server-2d+pin-joint-get-flag :class 'physics-server-2d :bind
  "pin_joint_get_flag" :hash 2647867364)
 bool (joint rid) (flag physics-server-2d+pin-joint-flag))

(defgmethod
 (physics-server-2d+pin-joint-set-param :class 'physics-server-2d :bind
  "pin_joint_set_param" :hash 550574241)
 :void (joint rid) (param physics-server-2d+pin-joint-param) (value float))

(defgmethod
 (physics-server-2d+pin-joint-get-param :class 'physics-server-2d :bind
  "pin_joint_get_param" :hash 348281383)
 float (joint rid) (param physics-server-2d+pin-joint-param))

(defgmethod
 (physics-server-2d+damped-spring-joint-set-param :class 'physics-server-2d
  :bind "damped_spring_joint_set_param" :hash 220564071)
 :void (joint rid) (param physics-server-2d+damped-spring-param) (value float))

(defgmethod
 (physics-server-2d+damped-spring-joint-get-param :class 'physics-server-2d
  :bind "damped_spring_joint_get_param" :hash 2075871277)
 float (joint rid) (param physics-server-2d+damped-spring-param))

(defgmethod
 (physics-server-2d+joint-get-type :class 'physics-server-2d :bind
  "joint_get_type" :hash 4262502231)
 physics-server-2d+joint-type (joint rid))

(defgmethod
 (physics-server-2d+free-rid :class 'physics-server-2d :bind "free_rid" :hash
  2722037293)
 :void (rid rid))

(defgmethod
 (physics-server-2d+set-active :class 'physics-server-2d :bind "set_active"
  :hash 2586408642)
 :void (active bool))

(defgmethod
 (physics-server-2d+get-process-info :class 'physics-server-2d :bind
  "get_process_info" :hash 576496006)
 int (process-info physics-server-2d+process-info))