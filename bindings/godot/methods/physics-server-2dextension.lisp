(common-lisp:in-package :%godot)


(defgmethod
 (physics-server-2dextension+%world-boundary-shape-create :class
  'physics-server-2dextension :bind "_world_boundary_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%separation-ray-shape-create :class
  'physics-server-2dextension :bind "_separation_ray_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%segment-shape-create :class
  'physics-server-2dextension :bind "_segment_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%circle-shape-create :class
  'physics-server-2dextension :bind "_circle_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%rectangle-shape-create :class
  'physics-server-2dextension :bind "_rectangle_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%capsule-shape-create :class
  'physics-server-2dextension :bind "_capsule_shape_create" :hash 529393457
  :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%convex-polygon-shape-create :class
  'physics-server-2dextension :bind "_convex_polygon_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%concave-polygon-shape-create :class
  'physics-server-2dextension :bind "_concave_polygon_shape_create" :hash
  529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%shape-set-data :class 'physics-server-2dextension
  :bind "_shape_set_data" :hash 3175752987 :virtual common-lisp:t)
 :void (shape rid) (data variant))

(defgmethod
 (physics-server-2dextension+%shape-set-custom-solver-bias :class
  'physics-server-2dextension :bind "_shape_set_custom_solver_bias" :hash
  1794382983 :virtual common-lisp:t)
 :void (shape rid) (bias float))

(defgmethod
 (physics-server-2dextension+%shape-get-type :class 'physics-server-2dextension
  :bind "_shape_get_type" :hash 1240598777 :virtual common-lisp:t)
 physics-server-2d+shape-type (shape rid))

(defgmethod
 (physics-server-2dextension+%shape-get-data :class 'physics-server-2dextension
  :bind "_shape_get_data" :hash 4171304767 :virtual common-lisp:t)
 variant (shape rid))

(defgmethod
 (physics-server-2dextension+%shape-get-custom-solver-bias :class
  'physics-server-2dextension :bind "_shape_get_custom_solver_bias" :hash
  866169185 :virtual common-lisp:t)
 float (shape rid))

(defgmethod
 (physics-server-2dextension+%shape-collide :class 'physics-server-2dextension
  :bind "_shape_collide" :hash 738864683 :virtual common-lisp:t)
 bool (shape-a rid) (xform-a transform-2d) (motion-a vector-2) (shape-b rid)
 (xform-b transform-2d) (motion-b vector-2) (r-results (:pointer :void))
 (result-max int) (r-result-count (:pointer :int32)))

(defgmethod
 (physics-server-2dextension+%space-create :class 'physics-server-2dextension
  :bind "_space_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%space-set-active :class
  'physics-server-2dextension :bind "_space_set_active" :hash 1265174801
  :virtual common-lisp:t)
 :void (space rid) (active bool))

(defgmethod
 (physics-server-2dextension+%space-is-active :class
  'physics-server-2dextension :bind "_space_is_active" :hash 4155700596
  :virtual common-lisp:t)
 bool (space rid))

(defgmethod
 (physics-server-2dextension+%space-set-param :class
  'physics-server-2dextension :bind "_space_set_param" :hash 949194586 :virtual
  common-lisp:t)
 :void (space rid) (param physics-server-2d+space-parameter) (value float))

(defgmethod
 (physics-server-2dextension+%space-get-param :class
  'physics-server-2dextension :bind "_space_get_param" :hash 874111783 :virtual
  common-lisp:t)
 float (space rid) (param physics-server-2d+space-parameter))

(defgmethod
 (physics-server-2dextension+%space-get-direct-state :class
  'physics-server-2dextension :bind "_space_get_direct_state" :hash 3160173886
  :virtual common-lisp:t)
 physics-direct-space-state-2d (space rid))

(defgmethod
 (physics-server-2dextension+%space-set-debug-contacts :class
  'physics-server-2dextension :bind "_space_set_debug_contacts" :hash
  3411492887 :virtual common-lisp:t)
 :void (space rid) (max-contacts int))

(defgmethod
 (physics-server-2dextension+%space-get-contacts :class
  'physics-server-2dextension :bind "_space_get_contacts" :hash 2222557395
  :virtual common-lisp:t)
 packed-vector-2array (space rid))

(defgmethod
 (physics-server-2dextension+%space-get-contact-count :class
  'physics-server-2dextension :bind "_space_get_contact_count" :hash 2198884583
  :virtual common-lisp:t)
 int (space rid))

(defgmethod
 (physics-server-2dextension+%area-create :class 'physics-server-2dextension
  :bind "_area_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%area-set-space :class 'physics-server-2dextension
  :bind "_area_set_space" :hash 395945892 :virtual common-lisp:t)
 :void (area rid) (space rid))

(defgmethod
 (physics-server-2dextension+%area-get-space :class 'physics-server-2dextension
  :bind "_area_get_space" :hash 3814569979 :virtual common-lisp:t)
 rid (area rid))

(defgmethod
 (physics-server-2dextension+%area-add-shape :class 'physics-server-2dextension
  :bind "_area_add_shape" :hash 888317420 :virtual common-lisp:t)
 :void (area rid) (shape rid) (transform transform-2d) (disabled bool))

(defgmethod
 (physics-server-2dextension+%area-set-shape :class 'physics-server-2dextension
  :bind "_area_set_shape" :hash 2310537182 :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-2dextension+%area-set-shape-transform :class
  'physics-server-2dextension :bind "_area_set_shape_transform" :hash 736082694
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (transform transform-2d))

(defgmethod
 (physics-server-2dextension+%area-set-shape-disabled :class
  'physics-server-2dextension :bind "_area_set_shape_disabled" :hash 2658558584
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-2dextension+%area-get-shape-count :class
  'physics-server-2dextension :bind "_area_get_shape_count" :hash 2198884583
  :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-2dextension+%area-get-shape :class 'physics-server-2dextension
  :bind "_area_get_shape" :hash 1066463050 :virtual common-lisp:t)
 rid (area rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%area-get-shape-transform :class
  'physics-server-2dextension :bind "_area_get_shape_transform" :hash
  1324854622 :virtual common-lisp:t)
 transform-2d (area rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%area-remove-shape :class
  'physics-server-2dextension :bind "_area_remove_shape" :hash 3411492887
  :virtual common-lisp:t)
 :void (area rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%area-clear-shapes :class
  'physics-server-2dextension :bind "_area_clear_shapes" :hash 2722037293
  :virtual common-lisp:t)
 :void (area rid))

(defgmethod
 (physics-server-2dextension+%area-attach-object-instance-id :class
  'physics-server-2dextension :bind "_area_attach_object_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (area rid) (id int))

(defgmethod
 (physics-server-2dextension+%area-get-object-instance-id :class
  'physics-server-2dextension :bind "_area_get_object_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-2dextension+%area-attach-canvas-instance-id :class
  'physics-server-2dextension :bind "_area_attach_canvas_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (area rid) (id int))

(defgmethod
 (physics-server-2dextension+%area-get-canvas-instance-id :class
  'physics-server-2dextension :bind "_area_get_canvas_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-2dextension+%area-set-param :class 'physics-server-2dextension
  :bind "_area_set_param" :hash 1257146028 :virtual common-lisp:t)
 :void (area rid) (param physics-server-2d+area-parameter) (value variant))

(defgmethod
 (physics-server-2dextension+%area-set-transform :class
  'physics-server-2dextension :bind "_area_set_transform" :hash 1246044741
  :virtual common-lisp:t)
 :void (area rid) (transform transform-2d))

(defgmethod
 (physics-server-2dextension+%area-get-param :class 'physics-server-2dextension
  :bind "_area_get_param" :hash 3047435120 :virtual common-lisp:t)
 variant (area rid) (param physics-server-2d+area-parameter))

(defgmethod
 (physics-server-2dextension+%area-get-transform :class
  'physics-server-2dextension :bind "_area_get_transform" :hash 213527486
  :virtual common-lisp:t)
 transform-2d (area rid))

(defgmethod
 (physics-server-2dextension+%area-set-collision-layer :class
  'physics-server-2dextension :bind "_area_set_collision_layer" :hash
  3411492887 :virtual common-lisp:t)
 :void (area rid) (layer int))

(defgmethod
 (physics-server-2dextension+%area-get-collision-layer :class
  'physics-server-2dextension :bind "_area_get_collision_layer" :hash
  2198884583 :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-2dextension+%area-set-collision-mask :class
  'physics-server-2dextension :bind "_area_set_collision_mask" :hash 3411492887
  :virtual common-lisp:t)
 :void (area rid) (mask int))

(defgmethod
 (physics-server-2dextension+%area-get-collision-mask :class
  'physics-server-2dextension :bind "_area_get_collision_mask" :hash 2198884583
  :virtual common-lisp:t)
 int (area rid))

(defgmethod
 (physics-server-2dextension+%area-set-monitorable :class
  'physics-server-2dextension :bind "_area_set_monitorable" :hash 1265174801
  :virtual common-lisp:t)
 :void (area rid) (monitorable bool))

(defgmethod
 (physics-server-2dextension+%area-set-pickable :class
  'physics-server-2dextension :bind "_area_set_pickable" :hash 1265174801
  :virtual common-lisp:t)
 :void (area rid) (pickable bool))

(defgmethod
 (physics-server-2dextension+%area-set-monitor-callback :class
  'physics-server-2dextension :bind "_area_set_monitor_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-2dextension+%area-set-area-monitor-callback :class
  'physics-server-2dextension :bind "_area_set_area_monitor_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (area rid) (callback callable))

(defgmethod
 (physics-server-2dextension+%body-create :class 'physics-server-2dextension
  :bind "_body_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%body-set-space :class 'physics-server-2dextension
  :bind "_body_set_space" :hash 395945892 :virtual common-lisp:t)
 :void (body rid) (space rid))

(defgmethod
 (physics-server-2dextension+%body-get-space :class 'physics-server-2dextension
  :bind "_body_get_space" :hash 3814569979 :virtual common-lisp:t)
 rid (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-mode :class 'physics-server-2dextension
  :bind "_body_set_mode" :hash 1658067650 :virtual common-lisp:t)
 :void (body rid) (mode physics-server-2d+body-mode))

(defgmethod
 (physics-server-2dextension+%body-get-mode :class 'physics-server-2dextension
  :bind "_body_get_mode" :hash 3261702585 :virtual common-lisp:t)
 physics-server-2d+body-mode (body rid))

(defgmethod
 (physics-server-2dextension+%body-add-shape :class 'physics-server-2dextension
  :bind "_body_add_shape" :hash 888317420 :virtual common-lisp:t)
 :void (body rid) (shape rid) (transform transform-2d) (disabled bool))

(defgmethod
 (physics-server-2dextension+%body-set-shape :class 'physics-server-2dextension
  :bind "_body_set_shape" :hash 2310537182 :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (shape rid))

(defgmethod
 (physics-server-2dextension+%body-set-shape-transform :class
  'physics-server-2dextension :bind "_body_set_shape_transform" :hash 736082694
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (transform transform-2d))

(defgmethod
 (physics-server-2dextension+%body-get-shape-count :class
  'physics-server-2dextension :bind "_body_get_shape_count" :hash 2198884583
  :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-get-shape :class 'physics-server-2dextension
  :bind "_body_get_shape" :hash 1066463050 :virtual common-lisp:t)
 rid (body rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%body-get-shape-transform :class
  'physics-server-2dextension :bind "_body_get_shape_transform" :hash
  1324854622 :virtual common-lisp:t)
 transform-2d (body rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%body-set-shape-disabled :class
  'physics-server-2dextension :bind "_body_set_shape_disabled" :hash 2658558584
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (disabled bool))

(defgmethod
 (physics-server-2dextension+%body-set-shape-as-one-way-collision :class
  'physics-server-2dextension :bind "_body_set_shape_as_one_way_collision"
  :hash 2042146392 :virtual common-lisp:t)
 :void (body rid) (shape-idx int) (enable bool) (margin float)
 (direction vector-2))

(defgmethod
 (physics-server-2dextension+%body-remove-shape :class
  'physics-server-2dextension :bind "_body_remove_shape" :hash 3411492887
  :virtual common-lisp:t)
 :void (body rid) (shape-idx int))

(defgmethod
 (physics-server-2dextension+%body-clear-shapes :class
  'physics-server-2dextension :bind "_body_clear_shapes" :hash 2722037293
  :virtual common-lisp:t)
 :void (body rid))

(defgmethod
 (physics-server-2dextension+%body-attach-object-instance-id :class
  'physics-server-2dextension :bind "_body_attach_object_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (id int))

(defgmethod
 (physics-server-2dextension+%body-get-object-instance-id :class
  'physics-server-2dextension :bind "_body_get_object_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-attach-canvas-instance-id :class
  'physics-server-2dextension :bind "_body_attach_canvas_instance_id" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (id int))

(defgmethod
 (physics-server-2dextension+%body-get-canvas-instance-id :class
  'physics-server-2dextension :bind "_body_get_canvas_instance_id" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-continuous-collision-detection-mode
  :class 'physics-server-2dextension :bind
  "_body_set_continuous_collision_detection_mode" :hash 1882257015 :virtual
  common-lisp:t)
 :void (body rid) (mode physics-server-2d+ccdmode))

(defgmethod
 (physics-server-2dextension+%body-get-continuous-collision-detection-mode
  :class 'physics-server-2dextension :bind
  "_body_get_continuous_collision_detection_mode" :hash 2661282217 :virtual
  common-lisp:t)
 physics-server-2d+ccdmode (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-collision-layer :class
  'physics-server-2dextension :bind "_body_set_collision_layer" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (layer int))

(defgmethod
 (physics-server-2dextension+%body-get-collision-layer :class
  'physics-server-2dextension :bind "_body_get_collision_layer" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-collision-mask :class
  'physics-server-2dextension :bind "_body_set_collision_mask" :hash 3411492887
  :virtual common-lisp:t)
 :void (body rid) (mask int))

(defgmethod
 (physics-server-2dextension+%body-get-collision-mask :class
  'physics-server-2dextension :bind "_body_get_collision_mask" :hash 2198884583
  :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-collision-priority :class
  'physics-server-2dextension :bind "_body_set_collision_priority" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (priority float))

(defgmethod
 (physics-server-2dextension+%body-get-collision-priority :class
  'physics-server-2dextension :bind "_body_get_collision_priority" :hash
  866169185 :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-param :class 'physics-server-2dextension
  :bind "_body_set_param" :hash 2715630609 :virtual common-lisp:t)
 :void (body rid) (param physics-server-2d+body-parameter) (value variant))

(defgmethod
 (physics-server-2dextension+%body-get-param :class 'physics-server-2dextension
  :bind "_body_get_param" :hash 3208033526 :virtual common-lisp:t)
 variant (body rid) (param physics-server-2d+body-parameter))

(defgmethod
 (physics-server-2dextension+%body-reset-mass-properties :class
  'physics-server-2dextension :bind "_body_reset_mass_properties" :hash
  2722037293 :virtual common-lisp:t)
 :void (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-state :class 'physics-server-2dextension
  :bind "_body_set_state" :hash 1706355209 :virtual common-lisp:t)
 :void (body rid) (state physics-server-2d+body-state) (value variant))

(defgmethod
 (physics-server-2dextension+%body-get-state :class 'physics-server-2dextension
  :bind "_body_get_state" :hash 4036367961 :virtual common-lisp:t)
 variant (body rid) (state physics-server-2d+body-state))

(defgmethod
 (physics-server-2dextension+%body-apply-central-impulse :class
  'physics-server-2dextension :bind "_body_apply_central_impulse" :hash
  3201125042 :virtual common-lisp:t)
 :void (body rid) (impulse vector-2))

(defgmethod
 (physics-server-2dextension+%body-apply-torque-impulse :class
  'physics-server-2dextension :bind "_body_apply_torque_impulse" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (impulse float))

(defgmethod
 (physics-server-2dextension+%body-apply-impulse :class
  'physics-server-2dextension :bind "_body_apply_impulse" :hash 2762675110
  :virtual common-lisp:t)
 :void (body rid) (impulse vector-2) (position vector-2))

(defgmethod
 (physics-server-2dextension+%body-apply-central-force :class
  'physics-server-2dextension :bind "_body_apply_central_force" :hash
  3201125042 :virtual common-lisp:t)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2dextension+%body-apply-force :class
  'physics-server-2dextension :bind "_body_apply_force" :hash 2762675110
  :virtual common-lisp:t)
 :void (body rid) (force vector-2) (position vector-2))

(defgmethod
 (physics-server-2dextension+%body-apply-torque :class
  'physics-server-2dextension :bind "_body_apply_torque" :hash 1794382983
  :virtual common-lisp:t)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2dextension+%body-add-constant-central-force :class
  'physics-server-2dextension :bind "_body_add_constant_central_force" :hash
  3201125042 :virtual common-lisp:t)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2dextension+%body-add-constant-force :class
  'physics-server-2dextension :bind "_body_add_constant_force" :hash 2762675110
  :virtual common-lisp:t)
 :void (body rid) (force vector-2) (position vector-2))

(defgmethod
 (physics-server-2dextension+%body-add-constant-torque :class
  'physics-server-2dextension :bind "_body_add_constant_torque" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2dextension+%body-set-constant-force :class
  'physics-server-2dextension :bind "_body_set_constant_force" :hash 3201125042
  :virtual common-lisp:t)
 :void (body rid) (force vector-2))

(defgmethod
 (physics-server-2dextension+%body-get-constant-force :class
  'physics-server-2dextension :bind "_body_get_constant_force" :hash 2440833711
  :virtual common-lisp:t)
 vector-2 (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-constant-torque :class
  'physics-server-2dextension :bind "_body_set_constant_torque" :hash
  1794382983 :virtual common-lisp:t)
 :void (body rid) (torque float))

(defgmethod
 (physics-server-2dextension+%body-get-constant-torque :class
  'physics-server-2dextension :bind "_body_get_constant_torque" :hash 866169185
  :virtual common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-axis-velocity :class
  'physics-server-2dextension :bind "_body_set_axis_velocity" :hash 3201125042
  :virtual common-lisp:t)
 :void (body rid) (axis-velocity vector-2))

(defgmethod
 (physics-server-2dextension+%body-add-collision-exception :class
  'physics-server-2dextension :bind "_body_add_collision_exception" :hash
  395945892 :virtual common-lisp:t)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-2dextension+%body-remove-collision-exception :class
  'physics-server-2dextension :bind "_body_remove_collision_exception" :hash
  395945892 :virtual common-lisp:t)
 :void (body rid) (excepted-body rid))

(defgmethod
 (physics-server-2dextension+%body-get-collision-exceptions :class
  'physics-server-2dextension :bind "_body_get_collision_exceptions" :hash
  2684255073 :virtual common-lisp:t)
 array (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-max-contacts-reported :class
  'physics-server-2dextension :bind "_body_set_max_contacts_reported" :hash
  3411492887 :virtual common-lisp:t)
 :void (body rid) (amount int))

(defgmethod
 (physics-server-2dextension+%body-get-max-contacts-reported :class
  'physics-server-2dextension :bind "_body_get_max_contacts_reported" :hash
  2198884583 :virtual common-lisp:t)
 int (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-contacts-reported-depth-threshold :class
  'physics-server-2dextension :bind
  "_body_set_contacts_reported_depth_threshold" :hash 1794382983 :virtual
  common-lisp:t)
 :void (body rid) (threshold float))

(defgmethod
 (physics-server-2dextension+%body-get-contacts-reported-depth-threshold :class
  'physics-server-2dextension :bind
  "_body_get_contacts_reported_depth_threshold" :hash 866169185 :virtual
  common-lisp:t)
 float (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-omit-force-integration :class
  'physics-server-2dextension :bind "_body_set_omit_force_integration" :hash
  1265174801 :virtual common-lisp:t)
 :void (body rid) (enable bool))

(defgmethod
 (physics-server-2dextension+%body-is-omitting-force-integration :class
  'physics-server-2dextension :bind "_body_is_omitting_force_integration" :hash
  4155700596 :virtual common-lisp:t)
 bool (body rid))

(defgmethod
 (physics-server-2dextension+%body-set-state-sync-callback :class
  'physics-server-2dextension :bind "_body_set_state_sync_callback" :hash
  3379118538 :virtual common-lisp:t)
 :void (body rid) (callable callable))

(defgmethod
 (physics-server-2dextension+%body-set-force-integration-callback :class
  'physics-server-2dextension :bind "_body_set_force_integration_callback"
  :hash 2828036238 :virtual common-lisp:t)
 :void (body rid) (callable callable) (userdata variant))

(defgmethod
 (physics-server-2dextension+%body-collide-shape :class
  'physics-server-2dextension :bind "_body_collide_shape" :hash 2131476465
  :virtual common-lisp:t)
 bool (body rid) (body-shape int) (shape rid) (shape-xform transform-2d)
 (motion vector-2) (r-results (:pointer :void)) (result-max int)
 (r-result-count (:pointer :int32)))

(defgmethod
 (physics-server-2dextension+%body-set-pickable :class
  'physics-server-2dextension :bind "_body_set_pickable" :hash 1265174801
  :virtual common-lisp:t)
 :void (body rid) (pickable bool))

(defgmethod
 (physics-server-2dextension+%body-get-direct-state :class
  'physics-server-2dextension :bind "_body_get_direct_state" :hash 1191931871
  :virtual common-lisp:t)
 physics-direct-body-state-2d (body rid))

(defgmethod
 (physics-server-2dextension+%body-test-motion :class
  'physics-server-2dextension :bind "_body_test_motion" :hash 104979818
  :virtual common-lisp:t)
 bool (body rid) (from transform-2d) (motion vector-2) (margin float)
 (collide-separation-ray bool) (recovery-as-collision bool)
 (r-result (:pointer physics-server-2dextension-motion-result)))

(defgmethod
 (physics-server-2dextension+%joint-create :class 'physics-server-2dextension
  :bind "_joint_create" :hash 529393457 :virtual common-lisp:t)
 rid)

(defgmethod
 (physics-server-2dextension+%joint-clear :class 'physics-server-2dextension
  :bind "_joint_clear" :hash 2722037293 :virtual common-lisp:t)
 :void (joint rid))

(defgmethod
 (physics-server-2dextension+%joint-set-param :class
  'physics-server-2dextension :bind "_joint_set_param" :hash 3972556514
  :virtual common-lisp:t)
 :void (joint rid) (param physics-server-2d+joint-param) (value float))

(defgmethod
 (physics-server-2dextension+%joint-get-param :class
  'physics-server-2dextension :bind "_joint_get_param" :hash 4016448949
  :virtual common-lisp:t)
 float (joint rid) (param physics-server-2d+joint-param))

(defgmethod
 (physics-server-2dextension+%joint-disable-collisions-between-bodies :class
  'physics-server-2dextension :bind "_joint_disable_collisions_between_bodies"
  :hash 1265174801 :virtual common-lisp:t)
 :void (joint rid) (disable bool))

(defgmethod
 (physics-server-2dextension+%joint-is-disabled-collisions-between-bodies
  :class 'physics-server-2dextension :bind
  "_joint_is_disabled_collisions_between_bodies" :hash 4155700596 :virtual
  common-lisp:t)
 bool (joint rid))

(defgmethod
 (physics-server-2dextension+%joint-make-pin :class 'physics-server-2dextension
  :bind "_joint_make_pin" :hash 2607799521 :virtual common-lisp:t)
 :void (joint rid) (anchor vector-2) (body-a rid) (body-b rid))

(defgmethod
 (physics-server-2dextension+%joint-make-groove :class
  'physics-server-2dextension :bind "_joint_make_groove" :hash 438649616
  :virtual common-lisp:t)
 :void (joint rid) (a-groove1 vector-2) (a-groove2 vector-2)
 (b-anchor vector-2) (body-a rid) (body-b rid))

(defgmethod
 (physics-server-2dextension+%joint-make-damped-spring :class
  'physics-server-2dextension :bind "_joint_make_damped_spring" :hash
  1276049561 :virtual common-lisp:t)
 :void (joint rid) (anchor-a vector-2) (anchor-b vector-2) (body-a rid)
 (body-b rid))

(defgmethod
 (physics-server-2dextension+%pin-joint-set-flag :class
  'physics-server-2dextension :bind "_pin_joint_set_flag" :hash 3520002352
  :virtual common-lisp:t)
 :void (joint rid) (flag physics-server-2d+pin-joint-flag) (enabled bool))

(defgmethod
 (physics-server-2dextension+%pin-joint-get-flag :class
  'physics-server-2dextension :bind "_pin_joint_get_flag" :hash 2647867364
  :virtual common-lisp:t)
 bool (joint rid) (flag physics-server-2d+pin-joint-flag))

(defgmethod
 (physics-server-2dextension+%pin-joint-set-param :class
  'physics-server-2dextension :bind "_pin_joint_set_param" :hash 550574241
  :virtual common-lisp:t)
 :void (joint rid) (param physics-server-2d+pin-joint-param) (value float))

(defgmethod
 (physics-server-2dextension+%pin-joint-get-param :class
  'physics-server-2dextension :bind "_pin_joint_get_param" :hash 348281383
  :virtual common-lisp:t)
 float (joint rid) (param physics-server-2d+pin-joint-param))

(defgmethod
 (physics-server-2dextension+%damped-spring-joint-set-param :class
  'physics-server-2dextension :bind "_damped_spring_joint_set_param" :hash
  220564071 :virtual common-lisp:t)
 :void (joint rid) (param physics-server-2d+damped-spring-param) (value float))

(defgmethod
 (physics-server-2dextension+%damped-spring-joint-get-param :class
  'physics-server-2dextension :bind "_damped_spring_joint_get_param" :hash
  2075871277 :virtual common-lisp:t)
 float (joint rid) (param physics-server-2d+damped-spring-param))

(defgmethod
 (physics-server-2dextension+%joint-get-type :class 'physics-server-2dextension
  :bind "_joint_get_type" :hash 4262502231 :virtual common-lisp:t)
 physics-server-2d+joint-type (joint rid))

(defgmethod
 (physics-server-2dextension+%free-rid :class 'physics-server-2dextension :bind
  "_free_rid" :hash 2722037293 :virtual common-lisp:t)
 :void (rid rid))

(defgmethod
 (physics-server-2dextension+%set-active :class 'physics-server-2dextension
  :bind "_set_active" :hash 2586408642 :virtual common-lisp:t)
 :void (active bool))

(defgmethod
 (physics-server-2dextension+%init :class 'physics-server-2dextension :bind
  "_init" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-2dextension+%step :class 'physics-server-2dextension :bind
  "_step" :hash 373806689 :virtual common-lisp:t)
 :void (step float))

(defgmethod
 (physics-server-2dextension+%sync :class 'physics-server-2dextension :bind
  "_sync" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-2dextension+%flush-queries :class 'physics-server-2dextension
  :bind "_flush_queries" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-2dextension+%end-sync :class 'physics-server-2dextension :bind
  "_end_sync" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-2dextension+%finish :class 'physics-server-2dextension :bind
  "_finish" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (physics-server-2dextension+%is-flushing-queries :class
  'physics-server-2dextension :bind "_is_flushing_queries" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (physics-server-2dextension+%get-process-info :class
  'physics-server-2dextension :bind "_get_process_info" :hash 576496006
  :virtual common-lisp:t)
 int (process-info physics-server-2d+process-info))

(defgmethod
 (physics-server-2dextension+body-test-motion-is-excluding-body :class
  'physics-server-2dextension :bind "body_test_motion_is_excluding_body" :hash
  4155700596)
 bool (body rid))

(defgmethod
 (physics-server-2dextension+body-test-motion-is-excluding-object :class
  'physics-server-2dextension :bind "body_test_motion_is_excluding_object"
  :hash 1116898809)
 bool (object int))