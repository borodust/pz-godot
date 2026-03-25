(common-lisp:in-package :%godot)


(defgmethod
 (lightmap-gi+set-light-data :class 'lightmap-gi :bind "set_light_data" :hash
  1790597277)
 :void (data lightmap-gidata))

(defgmethod
 (lightmap-gi+get-light-data :class 'lightmap-gi :bind "get_light_data" :hash
  290354153)
 lightmap-gidata)

(defgmethod
 (lightmap-gi+set-bake-quality :class 'lightmap-gi :bind "set_bake_quality"
  :hash 1192215803)
 :void (bake-quality lightmap-gi+bake-quality))

(defgmethod
 (lightmap-gi+get-bake-quality :class 'lightmap-gi :bind "get_bake_quality"
  :hash 688832735)
 lightmap-gi+bake-quality)

(defgmethod
 (lightmap-gi+set-bounces :class 'lightmap-gi :bind "set_bounces" :hash
  1286410249)
 :void (bounces int))

(defgmethod
 (lightmap-gi+get-bounces :class 'lightmap-gi :bind "get_bounces" :hash
  3905245786)
 int)

(defgmethod
 (lightmap-gi+set-bounce-indirect-energy :class 'lightmap-gi :bind
  "set_bounce_indirect_energy" :hash 373806689)
 :void (bounce-indirect-energy float))

(defgmethod
 (lightmap-gi+get-bounce-indirect-energy :class 'lightmap-gi :bind
  "get_bounce_indirect_energy" :hash 1740695150)
 float)

(defgmethod
 (lightmap-gi+set-generate-probes :class 'lightmap-gi :bind
  "set_generate_probes" :hash 549981046)
 :void (subdivision lightmap-gi+generate-probes))

(defgmethod
 (lightmap-gi+get-generate-probes :class 'lightmap-gi :bind
  "get_generate_probes" :hash 3930596226)
 lightmap-gi+generate-probes)

(defgmethod
 (lightmap-gi+set-bias :class 'lightmap-gi :bind "set_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (lightmap-gi+get-bias :class 'lightmap-gi :bind "get_bias" :hash 1740695150)
 float)

(defgmethod
 (lightmap-gi+set-environment-mode :class 'lightmap-gi :bind
  "set_environment_mode" :hash 2282650285)
 :void (mode lightmap-gi+environment-mode))

(defgmethod
 (lightmap-gi+get-environment-mode :class 'lightmap-gi :bind
  "get_environment_mode" :hash 4128646479)
 lightmap-gi+environment-mode)

(defgmethod
 (lightmap-gi+set-environment-custom-sky :class 'lightmap-gi :bind
  "set_environment_custom_sky" :hash 3336722921)
 :void (sky sky))

(defgmethod
 (lightmap-gi+get-environment-custom-sky :class 'lightmap-gi :bind
  "get_environment_custom_sky" :hash 1177136966)
 sky)

(defgmethod
 (lightmap-gi+set-environment-custom-color :class 'lightmap-gi :bind
  "set_environment_custom_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (lightmap-gi+get-environment-custom-color :class 'lightmap-gi :bind
  "get_environment_custom_color" :hash 3444240500)
 color)

(defgmethod
 (lightmap-gi+set-environment-custom-energy :class 'lightmap-gi :bind
  "set_environment_custom_energy" :hash 373806689)
 :void (energy float))

(defgmethod
 (lightmap-gi+get-environment-custom-energy :class 'lightmap-gi :bind
  "get_environment_custom_energy" :hash 1740695150)
 float)

(defgmethod
 (lightmap-gi+set-texel-scale :class 'lightmap-gi :bind "set_texel_scale" :hash
  373806689)
 :void (texel-scale float))

(defgmethod
 (lightmap-gi+get-texel-scale :class 'lightmap-gi :bind "get_texel_scale" :hash
  1740695150)
 float)

(defgmethod
 (lightmap-gi+set-max-texture-size :class 'lightmap-gi :bind
  "set_max_texture_size" :hash 1286410249)
 :void (max-texture-size int))

(defgmethod
 (lightmap-gi+get-max-texture-size :class 'lightmap-gi :bind
  "get_max_texture_size" :hash 3905245786)
 int)

(defgmethod
 (lightmap-gi+set-supersampling-enabled :class 'lightmap-gi :bind
  "set_supersampling_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (lightmap-gi+is-supersampling-enabled :class 'lightmap-gi :bind
  "is_supersampling_enabled" :hash 36873697)
 bool)

(defgmethod
 (lightmap-gi+set-supersampling-factor :class 'lightmap-gi :bind
  "set_supersampling_factor" :hash 373806689)
 :void (factor float))

(defgmethod
 (lightmap-gi+get-supersampling-factor :class 'lightmap-gi :bind
  "get_supersampling_factor" :hash 1740695150)
 float)

(defgmethod
 (lightmap-gi+set-use-denoiser :class 'lightmap-gi :bind "set_use_denoiser"
  :hash 2586408642)
 :void (use-denoiser bool))

(defgmethod
 (lightmap-gi+is-using-denoiser :class 'lightmap-gi :bind "is_using_denoiser"
  :hash 36873697)
 bool)

(defgmethod
 (lightmap-gi+set-denoiser-strength :class 'lightmap-gi :bind
  "set_denoiser_strength" :hash 373806689)
 :void (denoiser-strength float))

(defgmethod
 (lightmap-gi+get-denoiser-strength :class 'lightmap-gi :bind
  "get_denoiser_strength" :hash 1740695150)
 float)

(defgmethod
 (lightmap-gi+set-denoiser-range :class 'lightmap-gi :bind "set_denoiser_range"
  :hash 1286410249)
 :void (denoiser-range int))

(defgmethod
 (lightmap-gi+get-denoiser-range :class 'lightmap-gi :bind "get_denoiser_range"
  :hash 3905245786)
 int)

(defgmethod
 (lightmap-gi+set-interior :class 'lightmap-gi :bind "set_interior" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (lightmap-gi+is-interior :class 'lightmap-gi :bind "is_interior" :hash
  36873697)
 bool)

(defgmethod
 (lightmap-gi+set-directional :class 'lightmap-gi :bind "set_directional" :hash
  2586408642)
 :void (directional bool))

(defgmethod
 (lightmap-gi+is-directional :class 'lightmap-gi :bind "is_directional" :hash
  36873697)
 bool)

(defgmethod
 (lightmap-gi+set-shadowmask-mode :class 'lightmap-gi :bind
  "set_shadowmask_mode" :hash 3451066572)
 :void (mode lightmap-gidata+shadowmask-mode))

(defgmethod
 (lightmap-gi+get-shadowmask-mode :class 'lightmap-gi :bind
  "get_shadowmask_mode" :hash 785478560)
 lightmap-gidata+shadowmask-mode)

(defgmethod
 (lightmap-gi+set-use-texture-for-bounces :class 'lightmap-gi :bind
  "set_use_texture_for_bounces" :hash 2586408642)
 :void (use-texture-for-bounces bool))

(defgmethod
 (lightmap-gi+is-using-texture-for-bounces :class 'lightmap-gi :bind
  "is_using_texture_for_bounces" :hash 36873697)
 bool)

(defgmethod
 (lightmap-gi+set-camera-attributes :class 'lightmap-gi :bind
  "set_camera_attributes" :hash 2817810567)
 :void (camera-attributes camera-attributes))

(defgmethod
 (lightmap-gi+get-camera-attributes :class 'lightmap-gi :bind
  "get_camera_attributes" :hash 3921283215)
 camera-attributes)