(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-2d+set-emitting :class 'gpuparticles-2d :bind "set_emitting"
  :hash 2586408642)
 :void (emitting bool))

(defgmethod
 (gpuparticles-2d+set-amount :class 'gpuparticles-2d :bind "set_amount" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (gpuparticles-2d+set-lifetime :class 'gpuparticles-2d :bind "set_lifetime"
  :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-2d+set-one-shot :class 'gpuparticles-2d :bind "set_one_shot"
  :hash 2586408642)
 :void (secs bool))

(defgmethod
 (gpuparticles-2d+set-pre-process-time :class 'gpuparticles-2d :bind
  "set_pre_process_time" :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-2d+set-explosiveness-ratio :class 'gpuparticles-2d :bind
  "set_explosiveness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-2d+set-randomness-ratio :class 'gpuparticles-2d :bind
  "set_randomness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-2d+set-visibility-rect :class 'gpuparticles-2d :bind
  "set_visibility_rect" :hash 2046264180)
 :void (visibility-rect rect-2))

(defgmethod
 (gpuparticles-2d+set-use-local-coordinates :class 'gpuparticles-2d :bind
  "set_use_local_coordinates" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-2d+set-fixed-fps :class 'gpuparticles-2d :bind "set_fixed_fps"
  :hash 1286410249)
 :void (fps int))

(defgmethod
 (gpuparticles-2d+set-fractional-delta :class 'gpuparticles-2d :bind
  "set_fractional_delta" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-2d+set-interpolate :class 'gpuparticles-2d :bind
  "set_interpolate" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-2d+set-process-material :class 'gpuparticles-2d :bind
  "set_process_material" :hash 2757459619)
 :void (material material))

(defgmethod
 (gpuparticles-2d+set-speed-scale :class 'gpuparticles-2d :bind
  "set_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (gpuparticles-2d+set-collision-base-size :class 'gpuparticles-2d :bind
  "set_collision_base_size" :hash 373806689)
 :void (size float))

(defgmethod
 (gpuparticles-2d+set-interp-to-end :class 'gpuparticles-2d :bind
  "set_interp_to_end" :hash 373806689)
 :void (interp float))

(defgmethod
 (gpuparticles-2d+request-particles-process :class 'gpuparticles-2d :bind
  "request_particles_process" :hash 373806689)
 :void (process-time float))

(defgmethod
 (gpuparticles-2d+is-emitting :class 'gpuparticles-2d :bind "is_emitting" :hash
  36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-amount :class 'gpuparticles-2d :bind "get_amount" :hash
  3905245786)
 int)

(defgmethod
 (gpuparticles-2d+get-lifetime :class 'gpuparticles-2d :bind "get_lifetime"
  :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-one-shot :class 'gpuparticles-2d :bind "get_one_shot"
  :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-pre-process-time :class 'gpuparticles-2d :bind
  "get_pre_process_time" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-explosiveness-ratio :class 'gpuparticles-2d :bind
  "get_explosiveness_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-randomness-ratio :class 'gpuparticles-2d :bind
  "get_randomness_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-visibility-rect :class 'gpuparticles-2d :bind
  "get_visibility_rect" :hash 1639390495)
 rect-2)

(defgmethod
 (gpuparticles-2d+get-use-local-coordinates :class 'gpuparticles-2d :bind
  "get_use_local_coordinates" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-fixed-fps :class 'gpuparticles-2d :bind "get_fixed_fps"
  :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-2d+get-fractional-delta :class 'gpuparticles-2d :bind
  "get_fractional_delta" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-interpolate :class 'gpuparticles-2d :bind
  "get_interpolate" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-process-material :class 'gpuparticles-2d :bind
  "get_process_material" :hash 5934680)
 material)

(defgmethod
 (gpuparticles-2d+get-speed-scale :class 'gpuparticles-2d :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-collision-base-size :class 'gpuparticles-2d :bind
  "get_collision_base_size" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+get-interp-to-end :class 'gpuparticles-2d :bind
  "get_interp_to_end" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+set-draw-order :class 'gpuparticles-2d :bind "set_draw_order"
  :hash 1939677959)
 :void (order gpuparticles-2d+draw-order))

(defgmethod
 (gpuparticles-2d+get-draw-order :class 'gpuparticles-2d :bind "get_draw_order"
  :hash 941479095)
 gpuparticles-2d+draw-order)

(defgmethod
 (gpuparticles-2d+set-texture :class 'gpuparticles-2d :bind "set_texture" :hash
  4051416890)
 :void (texture texture-2d))

(defgmethod
 (gpuparticles-2d+get-texture :class 'gpuparticles-2d :bind "get_texture" :hash
  3635182373)
 texture-2d)

(defgmethod
 (gpuparticles-2d+capture-rect :class 'gpuparticles-2d :bind "capture_rect"
  :hash 1639390495)
 rect-2)

(defgmethod
 (gpuparticles-2d+restart :class 'gpuparticles-2d :bind "restart" :hash
  107499316)
 :void (keep-seed bool))

(defgmethod
 (gpuparticles-2d+set-sub-emitter :class 'gpuparticles-2d :bind
  "set_sub_emitter" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (gpuparticles-2d+get-sub-emitter :class 'gpuparticles-2d :bind
  "get_sub_emitter" :hash 4075236667)
 node-path)

(defgmethod
 (gpuparticles-2d+emit-particle :class 'gpuparticles-2d :bind "emit_particle"
  :hash 2179202058)
 :void (xform transform-2d) (velocity vector-2) (color color) (custom color)
 (flags int))

(defgmethod
 (gpuparticles-2d+set-trail-enabled :class 'gpuparticles-2d :bind
  "set_trail_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (gpuparticles-2d+set-trail-lifetime :class 'gpuparticles-2d :bind
  "set_trail_lifetime" :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-2d+is-trail-enabled :class 'gpuparticles-2d :bind
  "is_trail_enabled" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+get-trail-lifetime :class 'gpuparticles-2d :bind
  "get_trail_lifetime" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+set-trail-sections :class 'gpuparticles-2d :bind
  "set_trail_sections" :hash 1286410249)
 :void (sections int))

(defgmethod
 (gpuparticles-2d+get-trail-sections :class 'gpuparticles-2d :bind
  "get_trail_sections" :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-2d+set-trail-section-subdivisions :class 'gpuparticles-2d :bind
  "set_trail_section_subdivisions" :hash 1286410249)
 :void (subdivisions int))

(defgmethod
 (gpuparticles-2d+get-trail-section-subdivisions :class 'gpuparticles-2d :bind
  "get_trail_section_subdivisions" :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-2d+convert-from-particles :class 'gpuparticles-2d :bind
  "convert_from_particles" :hash 1078189570)
 :void (particles node))

(defgmethod
 (gpuparticles-2d+set-amount-ratio :class 'gpuparticles-2d :bind
  "set_amount_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-2d+get-amount-ratio :class 'gpuparticles-2d :bind
  "get_amount_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-2d+set-use-fixed-seed :class 'gpuparticles-2d :bind
  "set_use_fixed_seed" :hash 2586408642)
 :void (use-fixed-seed bool))

(defgmethod
 (gpuparticles-2d+get-use-fixed-seed :class 'gpuparticles-2d :bind
  "get_use_fixed_seed" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-2d+set-seed :class 'gpuparticles-2d :bind "set_seed" :hash
  1286410249)
 :void (seed int))

(defgmethod
 (gpuparticles-2d+get-seed :class 'gpuparticles-2d :bind "get_seed" :hash
  3905245786)
 int)