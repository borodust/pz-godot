(common-lisp:in-package :%godot)


(defgmethod
 (cpuparticles-2d+set-emitting :class 'cpuparticles-2d :bind "set_emitting"
  :hash 2586408642)
 :void (emitting bool))

(defgmethod
 (cpuparticles-2d+set-amount :class 'cpuparticles-2d :bind "set_amount" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (cpuparticles-2d+set-lifetime :class 'cpuparticles-2d :bind "set_lifetime"
  :hash 373806689)
 :void (secs float))

(defgmethod
 (cpuparticles-2d+set-one-shot :class 'cpuparticles-2d :bind "set_one_shot"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-2d+set-pre-process-time :class 'cpuparticles-2d :bind
  "set_pre_process_time" :hash 373806689)
 :void (secs float))

(defgmethod
 (cpuparticles-2d+set-explosiveness-ratio :class 'cpuparticles-2d :bind
  "set_explosiveness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (cpuparticles-2d+set-randomness-ratio :class 'cpuparticles-2d :bind
  "set_randomness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (cpuparticles-2d+set-lifetime-randomness :class 'cpuparticles-2d :bind
  "set_lifetime_randomness" :hash 373806689)
 :void (random float))

(defgmethod
 (cpuparticles-2d+set-use-local-coordinates :class 'cpuparticles-2d :bind
  "set_use_local_coordinates" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-2d+set-fixed-fps :class 'cpuparticles-2d :bind "set_fixed_fps"
  :hash 1286410249)
 :void (fps int))

(defgmethod
 (cpuparticles-2d+set-fractional-delta :class 'cpuparticles-2d :bind
  "set_fractional_delta" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (cpuparticles-2d+set-speed-scale :class 'cpuparticles-2d :bind
  "set_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (cpuparticles-2d+request-particles-process :class 'cpuparticles-2d :bind
  "request_particles_process" :hash 66938510)
 :void (process-time float) (process-time-residual float))

(defgmethod
 (cpuparticles-2d+is-emitting :class 'cpuparticles-2d :bind "is_emitting" :hash
  36873697)
 bool)

(defgmethod
 (cpuparticles-2d+get-amount :class 'cpuparticles-2d :bind "get_amount" :hash
  3905245786)
 int)

(defgmethod
 (cpuparticles-2d+get-lifetime :class 'cpuparticles-2d :bind "get_lifetime"
  :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-one-shot :class 'cpuparticles-2d :bind "get_one_shot"
  :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-2d+get-pre-process-time :class 'cpuparticles-2d :bind
  "get_pre_process_time" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-explosiveness-ratio :class 'cpuparticles-2d :bind
  "get_explosiveness_ratio" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-randomness-ratio :class 'cpuparticles-2d :bind
  "get_randomness_ratio" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-lifetime-randomness :class 'cpuparticles-2d :bind
  "get_lifetime_randomness" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-use-local-coordinates :class 'cpuparticles-2d :bind
  "get_use_local_coordinates" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-2d+get-fixed-fps :class 'cpuparticles-2d :bind "get_fixed_fps"
  :hash 3905245786)
 int)

(defgmethod
 (cpuparticles-2d+get-fractional-delta :class 'cpuparticles-2d :bind
  "get_fractional_delta" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-2d+get-speed-scale :class 'cpuparticles-2d :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+set-use-fixed-seed :class 'cpuparticles-2d :bind
  "set_use_fixed_seed" :hash 2586408642)
 :void (use-fixed-seed bool))

(defgmethod
 (cpuparticles-2d+get-use-fixed-seed :class 'cpuparticles-2d :bind
  "get_use_fixed_seed" :hash 36873697)
 bool)

(defgmethod
 (cpuparticles-2d+set-seed :class 'cpuparticles-2d :bind "set_seed" :hash
  1286410249)
 :void (seed int))

(defgmethod
 (cpuparticles-2d+get-seed :class 'cpuparticles-2d :bind "get_seed" :hash
  3905245786)
 int)

(defgmethod
 (cpuparticles-2d+set-draw-order :class 'cpuparticles-2d :bind "set_draw_order"
  :hash 4183193490)
 :void (order cpuparticles-2d+draw-order))

(defgmethod
 (cpuparticles-2d+get-draw-order :class 'cpuparticles-2d :bind "get_draw_order"
  :hash 1668655735)
 cpuparticles-2d+draw-order)

(defgmethod
 (cpuparticles-2d+set-texture :class 'cpuparticles-2d :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (cpuparticles-2d+get-texture :class 'cpuparticles-2d :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (cpuparticles-2d+restart :class 'cpuparticles-2d :bind "restart" :hash
  107499316)
 :void (keep-seed bool))

(defgmethod
 (cpuparticles-2d+set-direction :class 'cpuparticles-2d :bind "set_direction"
  :hash 743155724)
 :void (direction vector-2))

(defgmethod
 (cpuparticles-2d+get-direction :class 'cpuparticles-2d :bind "get_direction"
  :hash 3341600327)
 vector-2)

(defgmethod
 (cpuparticles-2d+set-spread :class 'cpuparticles-2d :bind "set_spread" :hash
  373806689)
 :void (spread float))

(defgmethod
 (cpuparticles-2d+get-spread :class 'cpuparticles-2d :bind "get_spread" :hash
  1740695150)
 float)

(defgmethod
 (cpuparticles-2d+set-param-min :class 'cpuparticles-2d :bind "set_param_min"
  :hash 3320615296)
 :void (param cpuparticles-2d+parameter) (value float))

(defgmethod
 (cpuparticles-2d+get-param-min :class 'cpuparticles-2d :bind "get_param_min"
  :hash 2038050600)
 float (param cpuparticles-2d+parameter))

(defgmethod
 (cpuparticles-2d+set-param-max :class 'cpuparticles-2d :bind "set_param_max"
  :hash 3320615296)
 :void (param cpuparticles-2d+parameter) (value float))

(defgmethod
 (cpuparticles-2d+get-param-max :class 'cpuparticles-2d :bind "get_param_max"
  :hash 2038050600)
 float (param cpuparticles-2d+parameter))

(defgmethod
 (cpuparticles-2d+set-param-curve :class 'cpuparticles-2d :bind
  "set_param_curve" :hash 2959350143)
 :void (param cpuparticles-2d+parameter) (curve curve))

(defgmethod
 (cpuparticles-2d+get-param-curve :class 'cpuparticles-2d :bind
  "get_param_curve" :hash 2603158474)
 curve (param cpuparticles-2d+parameter))

(defgmethod
 (cpuparticles-2d+set-color :class 'cpuparticles-2d :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (cpuparticles-2d+get-color :class 'cpuparticles-2d :bind "get_color" :hash
  3444240500)
 color)

(defgmethod
 (cpuparticles-2d+set-color-ramp :class 'cpuparticles-2d :bind "set_color_ramp"
  :hash 2756054477)
 :void (ramp gradient))

(defgmethod
 (cpuparticles-2d+get-color-ramp :class 'cpuparticles-2d :bind "get_color_ramp"
  :hash 132272999)
 gradient)

(defgmethod
 (cpuparticles-2d+set-color-initial-ramp :class 'cpuparticles-2d :bind
  "set_color_initial_ramp" :hash 2756054477)
 :void (ramp gradient))

(defgmethod
 (cpuparticles-2d+get-color-initial-ramp :class 'cpuparticles-2d :bind
  "get_color_initial_ramp" :hash 132272999)
 gradient)

(defgmethod
 (cpuparticles-2d+set-particle-flag :class 'cpuparticles-2d :bind
  "set_particle_flag" :hash 4178137949)
 :void (particle-flag cpuparticles-2d+particle-flags) (enable bool))

(defgmethod
 (cpuparticles-2d+get-particle-flag :class 'cpuparticles-2d :bind
  "get_particle_flag" :hash 2829976507)
 bool (particle-flag cpuparticles-2d+particle-flags))

(defgmethod
 (cpuparticles-2d+set-emission-shape :class 'cpuparticles-2d :bind
  "set_emission_shape" :hash 393763892)
 :void (shape cpuparticles-2d+emission-shape))

(defgmethod
 (cpuparticles-2d+get-emission-shape :class 'cpuparticles-2d :bind
  "get_emission_shape" :hash 1740246024)
 cpuparticles-2d+emission-shape)

(defgmethod
 (cpuparticles-2d+set-emission-sphere-radius :class 'cpuparticles-2d :bind
  "set_emission_sphere_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (cpuparticles-2d+get-emission-sphere-radius :class 'cpuparticles-2d :bind
  "get_emission_sphere_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+set-emission-rect-extents :class 'cpuparticles-2d :bind
  "set_emission_rect_extents" :hash 743155724)
 :void (extents vector-2))

(defgmethod
 (cpuparticles-2d+get-emission-rect-extents :class 'cpuparticles-2d :bind
  "get_emission_rect_extents" :hash 3341600327)
 vector-2)

(defgmethod
 (cpuparticles-2d+set-emission-points :class 'cpuparticles-2d :bind
  "set_emission_points" :hash 1509147220)
 :void (array packed-vector-2array))

(defgmethod
 (cpuparticles-2d+get-emission-points :class 'cpuparticles-2d :bind
  "get_emission_points" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (cpuparticles-2d+set-emission-normals :class 'cpuparticles-2d :bind
  "set_emission_normals" :hash 1509147220)
 :void (array packed-vector-2array))

(defgmethod
 (cpuparticles-2d+get-emission-normals :class 'cpuparticles-2d :bind
  "get_emission_normals" :hash 2961356807)
 packed-vector-2array)

(defgmethod
 (cpuparticles-2d+set-emission-colors :class 'cpuparticles-2d :bind
  "set_emission_colors" :hash 3546319833)
 :void (array packed-color-array))

(defgmethod
 (cpuparticles-2d+get-emission-colors :class 'cpuparticles-2d :bind
  "get_emission_colors" :hash 1392750486)
 packed-color-array)

(defgmethod
 (cpuparticles-2d+set-emission-ring-inner-radius :class 'cpuparticles-2d :bind
  "set_emission_ring_inner_radius" :hash 373806689)
 :void (inner-radius float))

(defgmethod
 (cpuparticles-2d+get-emission-ring-inner-radius :class 'cpuparticles-2d :bind
  "get_emission_ring_inner_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+set-emission-ring-radius :class 'cpuparticles-2d :bind
  "set_emission_ring_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (cpuparticles-2d+get-emission-ring-radius :class 'cpuparticles-2d :bind
  "get_emission_ring_radius" :hash 1740695150)
 float)

(defgmethod
 (cpuparticles-2d+get-gravity :class 'cpuparticles-2d :bind "get_gravity" :hash
  3341600327)
 vector-2)

(defgmethod
 (cpuparticles-2d+set-gravity :class 'cpuparticles-2d :bind "set_gravity" :hash
  743155724)
 :void (accel-vec vector-2))

(defgmethod
 (cpuparticles-2d+get-split-scale :class 'cpuparticles-2d :bind
  "get_split_scale" :hash 2240911060)
 bool)

(defgmethod
 (cpuparticles-2d+set-split-scale :class 'cpuparticles-2d :bind
  "set_split_scale" :hash 2586408642)
 :void (split-scale bool))

(defgmethod
 (cpuparticles-2d+get-scale-curve-x :class 'cpuparticles-2d :bind
  "get_scale_curve_x" :hash 2460114913)
 curve)

(defgmethod
 (cpuparticles-2d+set-scale-curve-x :class 'cpuparticles-2d :bind
  "set_scale_curve_x" :hash 270443179)
 :void (scale-curve curve))

(defgmethod
 (cpuparticles-2d+get-scale-curve-y :class 'cpuparticles-2d :bind
  "get_scale_curve_y" :hash 2460114913)
 curve)

(defgmethod
 (cpuparticles-2d+set-scale-curve-y :class 'cpuparticles-2d :bind
  "set_scale_curve_y" :hash 270443179)
 :void (scale-curve curve))

(defgmethod
 (cpuparticles-2d+convert-from-particles :class 'cpuparticles-2d :bind
  "convert_from_particles" :hash 1078189570)
 :void (particles node))