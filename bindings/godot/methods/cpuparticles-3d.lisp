(common-lisp:in-package :%godot)


(defgmethod
 (cpuparticles-3d+set-emitting :class 'cpuparticles-3d :bind "set_emitting"
  :hash 2586408642)
 :void (emitting bool))

(defgmethod
 (cpuparticles-3d+set-amount :class 'cpuparticles-3d :bind "set_amount" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (cpuparticles-3d+set-lifetime :class 'cpuparticles-3d :bind "set_lifetime"
  :hash 373806689)
 :void (secs float))

(defgmethod
 (cpuparticles-3d+set-one-shot :class 'cpuparticles-3d :bind "set_one_shot"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-3d+set-pre-process-time :class 'cpuparticles-3d :bind
  "set_pre_process_time" :hash 373806689)
 :void (secs float))

(defgmethod
 (cpuparticles-3d+set-explosiveness-ratio :class 'cpuparticles-3d :bind
  "set_explosiveness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (cpuparticles-3d+set-randomness-ratio :class 'cpuparticles-3d :bind
  "set_randomness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (cpuparticles-3d+set-visibility-aabb :class 'cpuparticles-3d :bind
  "set_visibility_aabb" :hash 259215842)
 :void (aabb aabb))

(defgmethod
 (cpuparticles-3d+set-lifetime-randomness :class 'cpuparticles-3d :bind
  "set_lifetime_randomness" :hash 373806689)
 :void (random float))

(defgmethod
 (cpuparticles-3d+set-use-local-coordinates :class 'cpuparticles-3d :bind
  "set_use_local_coordinates" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-3d+set-fixed-fps :class 'cpuparticles-3d :bind "set_fixed_fps"
  :hash 1286410249)
 :void (fps int))

(defgmethod
 (cpuparticles-3d+set-fractional-delta :class 'cpuparticles-3d :bind
  "set_fractional_delta" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-3d+set-speed-scale :class 'cpuparticles-3d :bind
  "set_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (cpuparticles-3d+is-emitting :class 'cpuparticles-3d :bind "is_emitting" :hash
  36873697)
 bool)

(defgmethod
 (cpuparticles-3d+get-amount :class 'cpuparticles-3d :bind "get_amount" :hash
  3905245786)
 int)

(defgmethod
 (cpuparticles-3d+get-lifetime :class 'cpuparticles-3d :bind "get_lifetime"
  :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-one-shot :class 'cpuparticles-3d :bind "get_one_shot"
  :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-3d+get-pre-process-time :class 'cpuparticles-3d :bind
  "get_pre_process_time" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-explosiveness-ratio :class 'cpuparticles-3d :bind
  "get_explosiveness_ratio" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-randomness-ratio :class 'cpuparticles-3d :bind
  "get_randomness_ratio" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-visibility-aabb :class 'cpuparticles-3d :bind
  "get_visibility_aabb" :hash 1068685055)
 aabb)

(defgmethod
 (cpuparticles-3d+get-lifetime-randomness :class 'cpuparticles-3d :bind
  "get_lifetime_randomness" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-use-local-coordinates :class 'cpuparticles-3d :bind
  "get_use_local_coordinates" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-3d+get-fixed-fps :class 'cpuparticles-3d :bind "get_fixed_fps"
  :hash 3905245786)
 int)

(defgmethod
 (cpuparticles-3d+get-fractional-delta :class 'cpuparticles-3d :bind
  "get_fractional_delta" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-3d+get-speed-scale :class 'cpuparticles-3d :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-draw-order :class 'cpuparticles-3d :bind "set_draw_order"
  :hash 1427401774)
 :void (order cpuparticles-3d+draw-order))

(defgmethod
 (cpuparticles-3d+get-draw-order :class 'cpuparticles-3d :bind "get_draw_order"
  :hash 1321900776)
 cpuparticles-3d+draw-order)

(defgmethod
 (cpuparticles-3d+set-mesh :class 'cpuparticles-3d :bind "set_mesh" :hash
  194775623)
 :void (mesh mesh))

(defgmethod
 (cpuparticles-3d+get-mesh :class 'cpuparticles-3d :bind "get_mesh" :hash
  1808005922)
 mesh)

(defgmethod
 (cpuparticles-3d+set-use-fixed-seed :class 'cpuparticles-3d :bind
  "set_use_fixed_seed" :hash 2586408642)
 :void (use-fixed-seed bool))

(defgmethod
 (cpuparticles-3d+get-use-fixed-seed :class 'cpuparticles-3d :bind
  "get_use_fixed_seed" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-3d+set-seed :class 'cpuparticles-3d :bind "set_seed" :hash
  1286410249)
 :void (seed int))

(defgmethod
 (cpuparticles-3d+get-seed :class 'cpuparticles-3d :bind "get_seed" :hash
  3905245786)
 int)

(defgmethod
 (cpuparticles-3d+restart :class 'cpuparticles-3d :bind "restart" :hash
  107499316)
 :void (keep-seed bool))

(defgmethod
 (cpuparticles-3d+request-particles-process :class 'cpuparticles-3d :bind
  "request_particles_process" :hash 66938510)
 :void (process-time float) (process-time-residual float))

(defgmethod
 (cpuparticles-3d+capture-aabb :class 'cpuparticles-3d :bind "capture_aabb"
  :hash 1068685055)
 aabb)

(defgmethod
 (cpuparticles-3d+set-direction :class 'cpuparticles-3d :bind "set_direction"
  :hash 3460891852)
 :void (direction vector-3))

(defgmethod
 (cpuparticles-3d+get-direction :class 'cpuparticles-3d :bind "get_direction"
  :hash 3360562783)
 vector-3)

(defgmethod
 (cpuparticles-3d+set-spread :class 'cpuparticles-3d :bind "set_spread" :hash
  373806689)
 :void (degrees float))

(defgmethod
 (cpuparticles-3d+get-spread :class 'cpuparticles-3d :bind "get_spread" :hash
  1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-flatness :class 'cpuparticles-3d :bind "set_flatness"
  :hash 373806689)
 :void (amount float))

(defgmethod
 (cpuparticles-3d+get-flatness :class 'cpuparticles-3d :bind "get_flatness"
  :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-param-min :class 'cpuparticles-3d :bind "set_param_min"
  :hash 557936109)
 :void (param cpuparticles-3d+parameter) (value float))

(defgmethod
 (cpuparticles-3d+get-param-min :class 'cpuparticles-3d :bind "get_param_min"
  :hash 597646162)
 float (param cpuparticles-3d+parameter))

(defgmethod
 (cpuparticles-3d+set-param-max :class 'cpuparticles-3d :bind "set_param_max"
  :hash 557936109)
 :void (param cpuparticles-3d+parameter) (value float))

(defgmethod
 (cpuparticles-3d+get-param-max :class 'cpuparticles-3d :bind "get_param_max"
  :hash 597646162)
 float (param cpuparticles-3d+parameter))

(defgmethod
 (cpuparticles-3d+set-param-curve :class 'cpuparticles-3d :bind
  "set_param_curve" :hash 4044142537)
 :void (param cpuparticles-3d+parameter) (curve curve))

(defgmethod
 (cpuparticles-3d+get-param-curve :class 'cpuparticles-3d :bind
  "get_param_curve" :hash 4132790277)
 curve (param cpuparticles-3d+parameter))

(defgmethod
 (cpuparticles-3d+set-color :class 'cpuparticles-3d :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (cpuparticles-3d+get-color :class 'cpuparticles-3d :bind "get_color" :hash
  3444240500)
 color)

(defgmethod
 (cpuparticles-3d+set-color-ramp :class 'cpuparticles-3d :bind "set_color_ramp"
  :hash 2756054477)
 :void (ramp gradient))

(defgmethod
 (cpuparticles-3d+get-color-ramp :class 'cpuparticles-3d :bind "get_color_ramp"
  :hash 132272999)
 gradient)

(defgmethod
 (cpuparticles-3d+set-color-initial-ramp :class 'cpuparticles-3d :bind
  "set_color_initial_ramp" :hash 2756054477)
 :void (ramp gradient))

(defgmethod
 (cpuparticles-3d+get-color-initial-ramp :class 'cpuparticles-3d :bind
  "get_color_initial_ramp" :hash 132272999)
 gradient)

(defgmethod
 (cpuparticles-3d+set-particle-flag :class 'cpuparticles-3d :bind
  "set_particle_flag" :hash 3515406498)
 :void (particle-flag cpuparticles-3d+particle-flags) (enable bool))

(defgmethod
 (cpuparticles-3d+get-particle-flag :class 'cpuparticles-3d :bind
  "get_particle_flag" :hash 2845201987)
 bool (particle-flag cpuparticles-3d+particle-flags))

(defgmethod
 (cpuparticles-3d+set-emission-shape :class 'cpuparticles-3d :bind
  "set_emission_shape" :hash 491823814)
 :void (shape cpuparticles-3d+emission-shape))

(defgmethod
 (cpuparticles-3d+get-emission-shape :class 'cpuparticles-3d :bind
  "get_emission_shape" :hash 2961454842)
 cpuparticles-3d+emission-shape)

(defgmethod
 (cpuparticles-3d+set-emission-sphere-radius :class 'cpuparticles-3d :bind
  "set_emission_sphere_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (cpuparticles-3d+get-emission-sphere-radius :class 'cpuparticles-3d :bind
  "get_emission_sphere_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-emission-box-extents :class 'cpuparticles-3d :bind
  "set_emission_box_extents" :hash 3460891852)
 :void (extents vector-3))

(defgmethod
 (cpuparticles-3d+get-emission-box-extents :class 'cpuparticles-3d :bind
  "get_emission_box_extents" :hash 3360562783)
 vector-3)

(defgmethod
 (cpuparticles-3d+set-emission-points :class 'cpuparticles-3d :bind
  "set_emission_points" :hash 334873810)
 :void (array packed-vector-3array))

(defgmethod
 (cpuparticles-3d+get-emission-points :class 'cpuparticles-3d :bind
  "get_emission_points" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (cpuparticles-3d+set-emission-normals :class 'cpuparticles-3d :bind
  "set_emission_normals" :hash 334873810)
 :void (array packed-vector-3array))

(defgmethod
 (cpuparticles-3d+get-emission-normals :class 'cpuparticles-3d :bind
  "get_emission_normals" :hash 497664490)
 packed-vector-3array)

(defgmethod
 (cpuparticles-3d+set-emission-colors :class 'cpuparticles-3d :bind
  "set_emission_colors" :hash 3546319833)
 :void (array packed-color-array))

(defgmethod
 (cpuparticles-3d+get-emission-colors :class 'cpuparticles-3d :bind
  "get_emission_colors" :hash 1392750486)
 packed-color-array)

(defgmethod
 (cpuparticles-3d+set-emission-ring-axis :class 'cpuparticles-3d :bind
  "set_emission_ring_axis" :hash 3460891852)
 :void (axis vector-3))

(defgmethod
 (cpuparticles-3d+get-emission-ring-axis :class 'cpuparticles-3d :bind
  "get_emission_ring_axis" :hash 3360562783)
 vector-3)

(defgmethod
 (cpuparticles-3d+set-emission-ring-height :class 'cpuparticles-3d :bind
  "set_emission_ring_height" :hash 373806689)
 :void (height float))

(defgmethod
 (cpuparticles-3d+get-emission-ring-height :class 'cpuparticles-3d :bind
  "get_emission_ring_height" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-emission-ring-radius :class 'cpuparticles-3d :bind
  "set_emission_ring_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (cpuparticles-3d+get-emission-ring-radius :class 'cpuparticles-3d :bind
  "get_emission_ring_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-emission-ring-inner-radius :class 'cpuparticles-3d :bind
  "set_emission_ring_inner_radius" :hash 373806689)
 :void (inner-radius float))

(defgmethod
 (cpuparticles-3d+get-emission-ring-inner-radius :class 'cpuparticles-3d :bind
  "get_emission_ring_inner_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+set-emission-ring-cone-angle :class 'cpuparticles-3d :bind
  "set_emission_ring_cone_angle" :hash 373806689)
 :void (cone-angle float))

(defgmethod
 (cpuparticles-3d+get-emission-ring-cone-angle :class 'cpuparticles-3d :bind
  "get_emission_ring_cone_angle" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-3d+get-gravity :class 'cpuparticles-3d :bind "get_gravity" :hash
  3360562783)
 vector-3)

(defgmethod
 (cpuparticles-3d+set-gravity :class 'cpuparticles-3d :bind "set_gravity" :hash
  3460891852)
 :void (accel-vec vector-3))

(defgmethod
 (cpuparticles-3d+get-split-scale :class 'cpuparticles-3d :bind
  "get_split_scale" :hash 2240911060)
 bool)

(defgmethod
 (cpuparticles-3d+set-split-scale :class 'cpuparticles-3d :bind
  "set_split_scale" :hash 2586408642)
 :void (split-scale bool))

(defgmethod
 (cpuparticles-3d+get-scale-curve-x :class 'cpuparticles-3d :bind
  "get_scale_curve_x" :hash 2460114913)
 curve)

(defgmethod
 (cpuparticles-3d+set-scale-curve-x :class 'cpuparticles-3d :bind
  "set_scale_curve_x" :hash 270443179)
 :void (scale-curve curve))

(defgmethod
 (cpuparticles-3d+get-scale-curve-y :class 'cpuparticles-3d :bind
  "get_scale_curve_y" :hash 2460114913)
 curve)

(defgmethod
 (cpuparticles-3d+set-scale-curve-y :class 'cpuparticles-3d :bind
  "set_scale_curve_y" :hash 270443179)
 :void (scale-curve curve))

(defgmethod
 (cpuparticles-3d+get-scale-curve-z :class 'cpuparticles-3d :bind
  "get_scale_curve_z" :hash 2460114913)
 curve)

(defgmethod
 (cpuparticles-3d+set-scale-curve-z :class 'cpuparticles-3d :bind
  "set_scale_curve_z" :hash 270443179)
 :void (scale-curve curve))

(defgmethod
 (cpuparticles-3d+convert-from-particles :class 'cpuparticles-3d :bind
  "convert_from_particles" :hash 1078189570)
 :void (particles node))