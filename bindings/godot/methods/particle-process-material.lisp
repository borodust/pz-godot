(common-lisp:in-package :%godot)


(defgmethod
 (particle-process-material+set-direction :class 'particle-process-material
  :bind "set_direction" :hash 3460891852)
 :void (degrees vector-3))

(defgmethod
 (particle-process-material+get-direction :class 'particle-process-material
  :bind "get_direction" :hash 3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-inherit-velocity-ratio :class
  'particle-process-material :bind "set_inherit_velocity_ratio" :hash
  373806689)
 :void (ratio float))

(defgmethod
 (particle-process-material+get-inherit-velocity-ratio :class
  'particle-process-material :bind "get_inherit_velocity_ratio" :hash
  191475506)
 float)

(defgmethod
 (particle-process-material+set-spread :class 'particle-process-material :bind
  "set_spread" :hash 373806689)
 :void (degrees float))

(defgmethod
 (particle-process-material+get-spread :class 'particle-process-material :bind
  "get_spread" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+set-flatness :class 'particle-process-material
  :bind "set_flatness" :hash 373806689)
 :void (amount float))

(defgmethod
 (particle-process-material+get-flatness :class 'particle-process-material
  :bind "get_flatness" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+set-param :class 'particle-process-material :bind
  "set_param" :hash 676779352)
 :void (param particle-process-material+parameter) (value vector-2))

(defgmethod
 (particle-process-material+get-param :class 'particle-process-material :bind
  "get_param" :hash 2623708480)
 vector-2 (param particle-process-material+parameter))

(defgmethod
 (particle-process-material+set-param-min :class 'particle-process-material
  :bind "set_param_min" :hash 2295964248)
 :void (param particle-process-material+parameter) (value float))

(defgmethod
 (particle-process-material+get-param-min :class 'particle-process-material
  :bind "get_param_min" :hash 3903786503)
 float (param particle-process-material+parameter))

(defgmethod
 (particle-process-material+set-param-max :class 'particle-process-material
  :bind "set_param_max" :hash 2295964248)
 :void (param particle-process-material+parameter) (value float))

(defgmethod
 (particle-process-material+get-param-max :class 'particle-process-material
  :bind "get_param_max" :hash 3903786503)
 float (param particle-process-material+parameter))

(defgmethod
 (particle-process-material+set-param-texture :class 'particle-process-material
  :bind "set_param_texture" :hash 526976089)
 :void (param particle-process-material+parameter) (texture texture-2d))

(defgmethod
 (particle-process-material+get-param-texture :class 'particle-process-material
  :bind "get_param_texture" :hash 3489372978)
 texture-2d (param particle-process-material+parameter))

(defgmethod
 (particle-process-material+set-color :class 'particle-process-material :bind
  "set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (particle-process-material+get-color :class 'particle-process-material :bind
  "get_color" :hash 3444240500)
 color)

(defgmethod
 (particle-process-material+set-color-ramp :class 'particle-process-material
  :bind "set_color_ramp" :hash 4051416890)
 :void (ramp texture-2d))

(defgmethod
 (particle-process-material+get-color-ramp :class 'particle-process-material
  :bind "get_color_ramp" :hash 3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-alpha-curve :class 'particle-process-material
  :bind "set_alpha_curve" :hash 4051416890)
 :void (curve texture-2d))

(defgmethod
 (particle-process-material+get-alpha-curve :class 'particle-process-material
  :bind "get_alpha_curve" :hash 3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-emission-curve :class
  'particle-process-material :bind "set_emission_curve" :hash 4051416890)
 :void (curve texture-2d))

(defgmethod
 (particle-process-material+get-emission-curve :class
  'particle-process-material :bind "get_emission_curve" :hash 3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-color-initial-ramp :class
  'particle-process-material :bind "set_color_initial_ramp" :hash 4051416890)
 :void (ramp texture-2d))

(defgmethod
 (particle-process-material+get-color-initial-ramp :class
  'particle-process-material :bind "get_color_initial_ramp" :hash 3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-velocity-limit-curve :class
  'particle-process-material :bind "set_velocity_limit_curve" :hash 4051416890)
 :void (curve texture-2d))

(defgmethod
 (particle-process-material+get-velocity-limit-curve :class
  'particle-process-material :bind "get_velocity_limit_curve" :hash 3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-particle-flag :class 'particle-process-material
  :bind "set_particle_flag" :hash 1711815571)
 :void (particle-flag particle-process-material+particle-flags) (enable bool))

(defgmethod
 (particle-process-material+get-particle-flag :class 'particle-process-material
  :bind "get_particle_flag" :hash 3895316907)
 bool (particle-flag particle-process-material+particle-flags))

(defgmethod
 (particle-process-material+set-velocity-pivot :class
  'particle-process-material :bind "set_velocity_pivot" :hash 3460891852)
 :void (pivot vector-3))

(defgmethod
 (particle-process-material+get-velocity-pivot :class
  'particle-process-material :bind "get_velocity_pivot" :hash 3783033775)
 vector-3)

(defgmethod
 (particle-process-material+set-emission-shape :class
  'particle-process-material :bind "set_emission_shape" :hash 461501442)
 :void (shape particle-process-material+emission-shape))

(defgmethod
 (particle-process-material+get-emission-shape :class
  'particle-process-material :bind "get_emission_shape" :hash 3719733018)
 particle-process-material+emission-shape)

(defgmethod
 (particle-process-material+set-emission-sphere-radius :class
  'particle-process-material :bind "set_emission_sphere_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (particle-process-material+get-emission-sphere-radius :class
  'particle-process-material :bind "get_emission_sphere_radius" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-emission-box-extents :class
  'particle-process-material :bind "set_emission_box_extents" :hash 3460891852)
 :void (extents vector-3))

(defgmethod
 (particle-process-material+get-emission-box-extents :class
  'particle-process-material :bind "get_emission_box_extents" :hash 3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-emission-point-texture :class
  'particle-process-material :bind "set_emission_point_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (particle-process-material+get-emission-point-texture :class
  'particle-process-material :bind "get_emission_point_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-emission-normal-texture :class
  'particle-process-material :bind "set_emission_normal_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (particle-process-material+get-emission-normal-texture :class
  'particle-process-material :bind "get_emission_normal_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-emission-color-texture :class
  'particle-process-material :bind "set_emission_color_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (particle-process-material+get-emission-color-texture :class
  'particle-process-material :bind "get_emission_color_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (particle-process-material+set-emission-point-count :class
  'particle-process-material :bind "set_emission_point_count" :hash 1286410249)
 :void (point-count int))

(defgmethod
 (particle-process-material+get-emission-point-count :class
  'particle-process-material :bind "get_emission_point_count" :hash 3905245786)
 int)

(defgmethod
 (particle-process-material+set-emission-ring-axis :class
  'particle-process-material :bind "set_emission_ring_axis" :hash 3460891852)
 :void (axis vector-3))

(defgmethod
 (particle-process-material+get-emission-ring-axis :class
  'particle-process-material :bind "get_emission_ring_axis" :hash 3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-emission-ring-height :class
  'particle-process-material :bind "set_emission_ring_height" :hash 373806689)
 :void (height float))

(defgmethod
 (particle-process-material+get-emission-ring-height :class
  'particle-process-material :bind "get_emission_ring_height" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+set-emission-ring-radius :class
  'particle-process-material :bind "set_emission_ring_radius" :hash 373806689)
 :void (radius float))

(defgmethod
 (particle-process-material+get-emission-ring-radius :class
  'particle-process-material :bind "get_emission_ring_radius" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+set-emission-ring-inner-radius :class
  'particle-process-material :bind "set_emission_ring_inner_radius" :hash
  373806689)
 :void (inner-radius float))

(defgmethod
 (particle-process-material+get-emission-ring-inner-radius :class
  'particle-process-material :bind "get_emission_ring_inner_radius" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-emission-ring-cone-angle :class
  'particle-process-material :bind "set_emission_ring_cone_angle" :hash
  373806689)
 :void (cone-angle float))

(defgmethod
 (particle-process-material+get-emission-ring-cone-angle :class
  'particle-process-material :bind "get_emission_ring_cone_angle" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-emission-shape-offset :class
  'particle-process-material :bind "set_emission_shape_offset" :hash
  3460891852)
 :void (emission-shape-offset vector-3))

(defgmethod
 (particle-process-material+get-emission-shape-offset :class
  'particle-process-material :bind "get_emission_shape_offset" :hash
  3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-emission-shape-scale :class
  'particle-process-material :bind "set_emission_shape_scale" :hash 3460891852)
 :void (emission-shape-scale vector-3))

(defgmethod
 (particle-process-material+get-emission-shape-scale :class
  'particle-process-material :bind "get_emission_shape_scale" :hash 3360562783)
 vector-3)

(defgmethod
 (particle-process-material+get-turbulence-enabled :class
  'particle-process-material :bind "get_turbulence_enabled" :hash 36873697)
 bool)

(defgmethod
 (particle-process-material+set-turbulence-enabled :class
  'particle-process-material :bind "set_turbulence_enabled" :hash 2586408642)
 :void (turbulence-enabled bool))

(defgmethod
 (particle-process-material+get-turbulence-noise-strength :class
  'particle-process-material :bind "get_turbulence_noise_strength" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-turbulence-noise-strength :class
  'particle-process-material :bind "set_turbulence_noise_strength" :hash
  373806689)
 :void (turbulence-noise-strength float))

(defgmethod
 (particle-process-material+get-turbulence-noise-scale :class
  'particle-process-material :bind "get_turbulence_noise_scale" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-turbulence-noise-scale :class
  'particle-process-material :bind "set_turbulence_noise_scale" :hash
  373806689)
 :void (turbulence-noise-scale float))

(defgmethod
 (particle-process-material+get-turbulence-noise-speed-random :class
  'particle-process-material :bind "get_turbulence_noise_speed_random" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-turbulence-noise-speed-random :class
  'particle-process-material :bind "set_turbulence_noise_speed_random" :hash
  373806689)
 :void (turbulence-noise-speed-random float))

(defgmethod
 (particle-process-material+get-turbulence-noise-speed :class
  'particle-process-material :bind "get_turbulence_noise_speed" :hash
  3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-turbulence-noise-speed :class
  'particle-process-material :bind "set_turbulence_noise_speed" :hash
  3460891852)
 :void (turbulence-noise-speed vector-3))

(defgmethod
 (particle-process-material+get-gravity :class 'particle-process-material :bind
  "get_gravity" :hash 3360562783)
 vector-3)

(defgmethod
 (particle-process-material+set-gravity :class 'particle-process-material :bind
  "set_gravity" :hash 3460891852)
 :void (accel-vec vector-3))

(defgmethod
 (particle-process-material+set-lifetime-randomness :class
  'particle-process-material :bind "set_lifetime_randomness" :hash 373806689)
 :void (randomness float))

(defgmethod
 (particle-process-material+get-lifetime-randomness :class
  'particle-process-material :bind "get_lifetime_randomness" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+get-sub-emitter-mode :class
  'particle-process-material :bind "get_sub_emitter_mode" :hash 2399052877)
 particle-process-material+sub-emitter-mode)

(defgmethod
 (particle-process-material+set-sub-emitter-mode :class
  'particle-process-material :bind "set_sub_emitter_mode" :hash 2161806672)
 :void (mode particle-process-material+sub-emitter-mode))

(defgmethod
 (particle-process-material+get-sub-emitter-frequency :class
  'particle-process-material :bind "get_sub_emitter_frequency" :hash
  1740695150)
 float)

(defgmethod
 (particle-process-material+set-sub-emitter-frequency :class
  'particle-process-material :bind "set_sub_emitter_frequency" :hash 373806689)
 :void (hz float))

(defgmethod
 (particle-process-material+get-sub-emitter-amount-at-end :class
  'particle-process-material :bind "get_sub_emitter_amount_at_end" :hash
  3905245786)
 int)

(defgmethod
 (particle-process-material+set-sub-emitter-amount-at-end :class
  'particle-process-material :bind "set_sub_emitter_amount_at_end" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (particle-process-material+get-sub-emitter-amount-at-collision :class
  'particle-process-material :bind "get_sub_emitter_amount_at_collision" :hash
  3905245786)
 int)

(defgmethod
 (particle-process-material+set-sub-emitter-amount-at-collision :class
  'particle-process-material :bind "set_sub_emitter_amount_at_collision" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (particle-process-material+get-sub-emitter-amount-at-start :class
  'particle-process-material :bind "get_sub_emitter_amount_at_start" :hash
  3905245786)
 int)

(defgmethod
 (particle-process-material+set-sub-emitter-amount-at-start :class
  'particle-process-material :bind "set_sub_emitter_amount_at_start" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (particle-process-material+get-sub-emitter-keep-velocity :class
  'particle-process-material :bind "get_sub_emitter_keep_velocity" :hash
  36873697)
 bool)

(defgmethod
 (particle-process-material+set-sub-emitter-keep-velocity :class
  'particle-process-material :bind "set_sub_emitter_keep_velocity" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (particle-process-material+set-attractor-interaction-enabled :class
  'particle-process-material :bind "set_attractor_interaction_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (particle-process-material+is-attractor-interaction-enabled :class
  'particle-process-material :bind "is_attractor_interaction_enabled" :hash
  36873697)
 bool)

(defgmethod
 (particle-process-material+set-collision-mode :class
  'particle-process-material :bind "set_collision_mode" :hash 653804659)
 :void (mode particle-process-material+collision-mode))

(defgmethod
 (particle-process-material+get-collision-mode :class
  'particle-process-material :bind "get_collision_mode" :hash 139371864)
 particle-process-material+collision-mode)

(defgmethod
 (particle-process-material+set-collision-use-scale :class
  'particle-process-material :bind "set_collision_use_scale" :hash 2586408642)
 :void (radius bool))

(defgmethod
 (particle-process-material+is-collision-using-scale :class
  'particle-process-material :bind "is_collision_using_scale" :hash 36873697)
 bool)

(defgmethod
 (particle-process-material+set-collision-friction :class
  'particle-process-material :bind "set_collision_friction" :hash 373806689)
 :void (friction float))

(defgmethod
 (particle-process-material+get-collision-friction :class
  'particle-process-material :bind "get_collision_friction" :hash 1740695150)
 float)

(defgmethod
 (particle-process-material+set-collision-bounce :class
  'particle-process-material :bind "set_collision_bounce" :hash 373806689)
 :void (bounce float))

(defgmethod
 (particle-process-material+get-collision-bounce :class
  'particle-process-material :bind "get_collision_bounce" :hash 1740695150)
 float)