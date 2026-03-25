(common-lisp:in-package :%godot)


(defgmethod
 (rdpipeline-rasterization-state+set-enable-depth-clamp :class
  'rdpipeline-rasterization-state :bind "set_enable_depth_clamp" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-rasterization-state+get-enable-depth-clamp :class
  'rdpipeline-rasterization-state :bind "get_enable_depth_clamp" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-rasterization-state+set-discard-primitives :class
  'rdpipeline-rasterization-state :bind "set_discard_primitives" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-rasterization-state+get-discard-primitives :class
  'rdpipeline-rasterization-state :bind "get_discard_primitives" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-rasterization-state+set-wireframe :class
  'rdpipeline-rasterization-state :bind "set_wireframe" :hash 2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-rasterization-state+get-wireframe :class
  'rdpipeline-rasterization-state :bind "get_wireframe" :hash 36873697)
 bool)

(defgmethod
 (rdpipeline-rasterization-state+set-cull-mode :class
  'rdpipeline-rasterization-state :bind "set_cull_mode" :hash 2662586502)
 :void (p-member rendering-device+polygon-cull-mode))

(defgmethod
 (rdpipeline-rasterization-state+get-cull-mode :class
  'rdpipeline-rasterization-state :bind "get_cull_mode" :hash 2192484313)
 rendering-device+polygon-cull-mode)

(defgmethod
 (rdpipeline-rasterization-state+set-front-face :class
  'rdpipeline-rasterization-state :bind "set_front_face" :hash 2637251213)
 :void (p-member rendering-device+polygon-front-face))

(defgmethod
 (rdpipeline-rasterization-state+get-front-face :class
  'rdpipeline-rasterization-state :bind "get_front_face" :hash 708793786)
 rendering-device+polygon-front-face)

(defgmethod
 (rdpipeline-rasterization-state+set-depth-bias-enabled :class
  'rdpipeline-rasterization-state :bind "set_depth_bias_enabled" :hash
  2586408642)
 :void (p-member bool))

(defgmethod
 (rdpipeline-rasterization-state+get-depth-bias-enabled :class
  'rdpipeline-rasterization-state :bind "get_depth_bias_enabled" :hash
  36873697)
 bool)

(defgmethod
 (rdpipeline-rasterization-state+set-depth-bias-constant-factor :class
  'rdpipeline-rasterization-state :bind "set_depth_bias_constant_factor" :hash
  373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-rasterization-state+get-depth-bias-constant-factor :class
  'rdpipeline-rasterization-state :bind "get_depth_bias_constant_factor" :hash
  1740695150)
 float)

(defgmethod
 (rdpipeline-rasterization-state+set-depth-bias-clamp :class
  'rdpipeline-rasterization-state :bind "set_depth_bias_clamp" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-rasterization-state+get-depth-bias-clamp :class
  'rdpipeline-rasterization-state :bind "get_depth_bias_clamp" :hash
  1740695150)
 float)

(defgmethod
 (rdpipeline-rasterization-state+set-depth-bias-slope-factor :class
  'rdpipeline-rasterization-state :bind "set_depth_bias_slope_factor" :hash
  373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-rasterization-state+get-depth-bias-slope-factor :class
  'rdpipeline-rasterization-state :bind "get_depth_bias_slope_factor" :hash
  1740695150)
 float)

(defgmethod
 (rdpipeline-rasterization-state+set-line-width :class
  'rdpipeline-rasterization-state :bind "set_line_width" :hash 373806689)
 :void (p-member float))

(defgmethod
 (rdpipeline-rasterization-state+get-line-width :class
  'rdpipeline-rasterization-state :bind "get_line_width" :hash 1740695150)
 float)

(defgmethod
 (rdpipeline-rasterization-state+set-patch-control-points :class
  'rdpipeline-rasterization-state :bind "set_patch_control_points" :hash
  1286410249)
 :void (p-member int))

(defgmethod
 (rdpipeline-rasterization-state+get-patch-control-points :class
  'rdpipeline-rasterization-state :bind "get_patch_control_points" :hash
  3905245786)
 int)