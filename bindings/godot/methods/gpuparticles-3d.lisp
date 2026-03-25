(common-lisp:in-package :%godot)


(defgmethod
 (gpuparticles-3d+set-emitting :class 'gpuparticles-3d :bind "set_emitting"
  :hash 2586408642)
 :void (emitting bool))

(defgmethod
 (gpuparticles-3d+set-amount :class 'gpuparticles-3d :bind "set_amount" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (gpuparticles-3d+set-lifetime :class 'gpuparticles-3d :bind "set_lifetime"
  :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-3d+set-one-shot :class 'gpuparticles-3d :bind "set_one_shot"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-3d+set-pre-process-time :class 'gpuparticles-3d :bind
  "set_pre_process_time" :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-3d+set-explosiveness-ratio :class 'gpuparticles-3d :bind
  "set_explosiveness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-3d+set-randomness-ratio :class 'gpuparticles-3d :bind
  "set_randomness_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-3d+set-visibility-aabb :class 'gpuparticles-3d :bind
  "set_visibility_aabb" :hash 259215842)
 :void (aabb aabb))

(defgmethod
 (gpuparticles-3d+set-use-local-coordinates :class 'gpuparticles-3d :bind
  "set_use_local_coordinates" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-3d+set-fixed-fps :class 'gpuparticles-3d :bind "set_fixed_fps"
  :hash 1286410249)
 :void (fps int))

(defgmethod
 (gpuparticles-3d+set-fractional-delta :class 'gpuparticles-3d :bind
  "set_fractional_delta" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-3d+set-interpolate :class 'gpuparticles-3d :bind
  "set_interpolate" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (gpuparticles-3d+set-process-material :class 'gpuparticles-3d :bind
  "set_process_material" :hash 2757459619)
 :void (material material))

(defgmethod
 (gpuparticles-3d+set-speed-scale :class 'gpuparticles-3d :bind
  "set_speed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (gpuparticles-3d+set-collision-base-size :class 'gpuparticles-3d :bind
  "set_collision_base_size" :hash 373806689)
 :void (size float))

(defgmethod
 (gpuparticles-3d+set-interp-to-end :class 'gpuparticles-3d :bind
  "set_interp_to_end" :hash 373806689)
 :void (interp float))

(defgmethod
 (gpuparticles-3d+is-emitting :class 'gpuparticles-3d :bind "is_emitting" :hash
  36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-amount :class 'gpuparticles-3d :bind "get_amount" :hash
  3905245786)
 int)

(defgmethod
 (gpuparticles-3d+get-lifetime :class 'gpuparticles-3d :bind "get_lifetime"
  :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-one-shot :class 'gpuparticles-3d :bind "get_one_shot"
  :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-pre-process-time :class 'gpuparticles-3d :bind
  "get_pre_process_time" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-explosiveness-ratio :class 'gpuparticles-3d :bind
  "get_explosiveness_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-randomness-ratio :class 'gpuparticles-3d :bind
  "get_randomness_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-visibility-aabb :class 'gpuparticles-3d :bind
  "get_visibility_aabb" :hash 1068685055)
 aabb)

(defgmethod
 (gpuparticles-3d+get-use-local-coordinates :class 'gpuparticles-3d :bind
  "get_use_local_coordinates" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-fixed-fps :class 'gpuparticles-3d :bind "get_fixed_fps"
  :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-3d+get-fractional-delta :class 'gpuparticles-3d :bind
  "get_fractional_delta" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-interpolate :class 'gpuparticles-3d :bind
  "get_interpolate" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-process-material :class 'gpuparticles-3d :bind
  "get_process_material" :hash 5934680)
 material)

(defgmethod
 (gpuparticles-3d+get-speed-scale :class 'gpuparticles-3d :bind
  "get_speed_scale" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-collision-base-size :class 'gpuparticles-3d :bind
  "get_collision_base_size" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+get-interp-to-end :class 'gpuparticles-3d :bind
  "get_interp_to_end" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+set-use-fixed-seed :class 'gpuparticles-3d :bind
  "set_use_fixed_seed" :hash 2586408642)
 :void (use-fixed-seed bool))

(defgmethod
 (gpuparticles-3d+get-use-fixed-seed :class 'gpuparticles-3d :bind
  "get_use_fixed_seed" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+set-seed :class 'gpuparticles-3d :bind "set_seed" :hash
  1286410249)
 :void (seed int))

(defgmethod
 (gpuparticles-3d+get-seed :class 'gpuparticles-3d :bind "get_seed" :hash
  3905245786)
 int)

(defgmethod
 (gpuparticles-3d+set-draw-order :class 'gpuparticles-3d :bind "set_draw_order"
  :hash 1208074815)
 :void (order gpuparticles-3d+draw-order))

(defgmethod
 (gpuparticles-3d+get-draw-order :class 'gpuparticles-3d :bind "get_draw_order"
  :hash 3770381780)
 gpuparticles-3d+draw-order)

(defgmethod
 (gpuparticles-3d+set-draw-passes :class 'gpuparticles-3d :bind
  "set_draw_passes" :hash 1286410249)
 :void (passes int))

(defgmethod
 (gpuparticles-3d+set-draw-pass-mesh :class 'gpuparticles-3d :bind
  "set_draw_pass_mesh" :hash 969122797)
 :void (pass int) (mesh mesh))

(defgmethod
 (gpuparticles-3d+get-draw-passes :class 'gpuparticles-3d :bind
  "get_draw_passes" :hash 3905245786)
 int)

(defgmethod
 (gpuparticles-3d+get-draw-pass-mesh :class 'gpuparticles-3d :bind
  "get_draw_pass_mesh" :hash 1576363275)
 mesh (pass int))

(defgmethod
 (gpuparticles-3d+set-skin :class 'gpuparticles-3d :bind "set_skin" :hash
  3971435618)
 :void (skin skin))

(defgmethod
 (gpuparticles-3d+get-skin :class 'gpuparticles-3d :bind "get_skin" :hash
  2074563878)
 skin)

(defgmethod
 (gpuparticles-3d+restart :class 'gpuparticles-3d :bind "restart" :hash
  107499316)
 :void (keep-seed bool))

(defgmethod
 (gpuparticles-3d+capture-aabb :class 'gpuparticles-3d :bind "capture_aabb"
  :hash 1068685055)
 aabb)

(defgmethod
 (gpuparticles-3d+set-sub-emitter :class 'gpuparticles-3d :bind
  "set_sub_emitter" :hash 1348162250)
 :void (path node-path))

(defgmethod
 (gpuparticles-3d+get-sub-emitter :class 'gpuparticles-3d :bind
  "get_sub_emitter" :hash 4075236667)
 node-path)

(defgmethod
 (gpuparticles-3d+emit-particle :class 'gpuparticles-3d :bind "emit_particle"
  :hash 992173727)
 :void (xform transform-3d) (velocity vector-3) (color color) (custom color)
 (flags int))

(defgmethod
 (gpuparticles-3d+set-trail-enabled :class 'gpuparticles-3d :bind
  "set_trail_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (gpuparticles-3d+set-trail-lifetime :class 'gpuparticles-3d :bind
  "set_trail_lifetime" :hash 373806689)
 :void (secs float))

(defgmethod
 (gpuparticles-3d+is-trail-enabled :class 'gpuparticles-3d :bind
  "is_trail_enabled" :hash 36873697)
 bool)

(defgmethod
 (gpuparticles-3d+get-trail-lifetime :class 'gpuparticles-3d :bind
  "get_trail_lifetime" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+set-transform-align :class 'gpuparticles-3d :bind
  "set_transform_align" :hash 3892425954)
 :void (align gpuparticles-3d+transform-align))

(defgmethod
 (gpuparticles-3d+get-transform-align :class 'gpuparticles-3d :bind
  "get_transform_align" :hash 2100992166)
 gpuparticles-3d+transform-align)

(defgmethod
 (gpuparticles-3d+convert-from-particles :class 'gpuparticles-3d :bind
  "convert_from_particles" :hash 1078189570)
 :void (particles node))

(defgmethod
 (gpuparticles-3d+set-amount-ratio :class 'gpuparticles-3d :bind
  "set_amount_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (gpuparticles-3d+get-amount-ratio :class 'gpuparticles-3d :bind
  "get_amount_ratio" :hash 1740695150)
 float)

(defgmethod
 (gpuparticles-3d+request-particles-process :class 'gpuparticles-3d :bind
  "request_particles_process" :hash 373806689)
 :void (process-time float))