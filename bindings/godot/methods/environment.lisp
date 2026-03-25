(common-lisp:in-package :%godot)


(defgmethod
 (environment+set-background :class 'environment :bind "set_background" :hash
  4071623990)
 :void (mode environment+bgmode))

(defgmethod
 (environment+get-background :class 'environment :bind "get_background" :hash
  1843210413)
 environment+bgmode)

(defgmethod
 (environment+set-sky :class 'environment :bind "set_sky" :hash 3336722921)
 :void (sky sky))

(defgmethod
 (environment+get-sky :class 'environment :bind "get_sky" :hash 1177136966) sky)

(defgmethod
 (environment+set-sky-custom-fov :class 'environment :bind "set_sky_custom_fov"
  :hash 373806689)
 :void (scale float))

(defgmethod
 (environment+get-sky-custom-fov :class 'environment :bind "get_sky_custom_fov"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-sky-rotation :class 'environment :bind "set_sky_rotation"
  :hash 3460891852)
 :void (euler-radians vector-3))

(defgmethod
 (environment+get-sky-rotation :class 'environment :bind "get_sky_rotation"
  :hash 3360562783)
 vector-3)

(defgmethod
 (environment+set-bg-color :class 'environment :bind "set_bg_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (environment+get-bg-color :class 'environment :bind "get_bg_color" :hash
  3444240500)
 color)

(defgmethod
 (environment+set-bg-energy-multiplier :class 'environment :bind
  "set_bg_energy_multiplier" :hash 373806689)
 :void (energy float))

(defgmethod
 (environment+get-bg-energy-multiplier :class 'environment :bind
  "get_bg_energy_multiplier" :hash 1740695150)
 float)

(defgmethod
 (environment+set-bg-intensity :class 'environment :bind "set_bg_intensity"
  :hash 373806689)
 :void (energy float))

(defgmethod
 (environment+get-bg-intensity :class 'environment :bind "get_bg_intensity"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-canvas-max-layer :class 'environment :bind
  "set_canvas_max_layer" :hash 1286410249)
 :void (layer int))

(defgmethod
 (environment+get-canvas-max-layer :class 'environment :bind
  "get_canvas_max_layer" :hash 3905245786)
 int)

(defgmethod
 (environment+set-camera-feed-id :class 'environment :bind "set_camera_feed_id"
  :hash 1286410249)
 :void (id int))

(defgmethod
 (environment+get-camera-feed-id :class 'environment :bind "get_camera_feed_id"
  :hash 3905245786)
 int)

(defgmethod
 (environment+set-ambient-light-color :class 'environment :bind
  "set_ambient_light_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (environment+get-ambient-light-color :class 'environment :bind
  "get_ambient_light_color" :hash 3444240500)
 color)

(defgmethod
 (environment+set-ambient-source :class 'environment :bind "set_ambient_source"
  :hash 2607780160)
 :void (source environment+ambient-source))

(defgmethod
 (environment+get-ambient-source :class 'environment :bind "get_ambient_source"
  :hash 67453933)
 environment+ambient-source)

(defgmethod
 (environment+set-ambient-light-energy :class 'environment :bind
  "set_ambient_light_energy" :hash 373806689)
 :void (energy float))

(defgmethod
 (environment+get-ambient-light-energy :class 'environment :bind
  "get_ambient_light_energy" :hash 1740695150)
 float)

(defgmethod
 (environment+set-ambient-light-sky-contribution :class 'environment :bind
  "set_ambient_light_sky_contribution" :hash 373806689)
 :void (ratio float))

(defgmethod
 (environment+get-ambient-light-sky-contribution :class 'environment :bind
  "get_ambient_light_sky_contribution" :hash 1740695150)
 float)

(defgmethod
 (environment+set-reflection-source :class 'environment :bind
  "set_reflection_source" :hash 299673197)
 :void (source environment+reflection-source))

(defgmethod
 (environment+get-reflection-source :class 'environment :bind
  "get_reflection_source" :hash 777700713)
 environment+reflection-source)

(defgmethod
 (environment+set-tonemapper :class 'environment :bind "set_tonemapper" :hash
  1509116664)
 :void (mode environment+tone-mapper))

(defgmethod
 (environment+get-tonemapper :class 'environment :bind "get_tonemapper" :hash
  2908408137)
 environment+tone-mapper)

(defgmethod
 (environment+set-tonemap-exposure :class 'environment :bind
  "set_tonemap_exposure" :hash 373806689)
 :void (exposure float))

(defgmethod
 (environment+get-tonemap-exposure :class 'environment :bind
  "get_tonemap_exposure" :hash 1740695150)
 float)

(defgmethod
 (environment+set-tonemap-white :class 'environment :bind "set_tonemap_white"
  :hash 373806689)
 :void (white float))

(defgmethod
 (environment+get-tonemap-white :class 'environment :bind "get_tonemap_white"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-tonemap-agx-white :class 'environment :bind
  "set_tonemap_agx_white" :hash 373806689)
 :void (white float))

(defgmethod
 (environment+get-tonemap-agx-white :class 'environment :bind
  "get_tonemap_agx_white" :hash 1740695150)
 float)

(defgmethod
 (environment+set-tonemap-agx-contrast :class 'environment :bind
  "set_tonemap_agx_contrast" :hash 373806689)
 :void (contrast float))

(defgmethod
 (environment+get-tonemap-agx-contrast :class 'environment :bind
  "get_tonemap_agx_contrast" :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssr-enabled :class 'environment :bind "set_ssr_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-ssr-enabled :class 'environment :bind "is_ssr_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-ssr-max-steps :class 'environment :bind "set_ssr_max_steps"
  :hash 1286410249)
 :void (max-steps int))

(defgmethod
 (environment+get-ssr-max-steps :class 'environment :bind "get_ssr_max_steps"
  :hash 3905245786)
 int)

(defgmethod
 (environment+set-ssr-fade-in :class 'environment :bind "set_ssr_fade_in" :hash
  373806689)
 :void (fade-in float))

(defgmethod
 (environment+get-ssr-fade-in :class 'environment :bind "get_ssr_fade_in" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-ssr-fade-out :class 'environment :bind "set_ssr_fade_out"
  :hash 373806689)
 :void (fade-out float))

(defgmethod
 (environment+get-ssr-fade-out :class 'environment :bind "get_ssr_fade_out"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssr-depth-tolerance :class 'environment :bind
  "set_ssr_depth_tolerance" :hash 373806689)
 :void (depth-tolerance float))

(defgmethod
 (environment+get-ssr-depth-tolerance :class 'environment :bind
  "get_ssr_depth_tolerance" :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssao-enabled :class 'environment :bind "set_ssao_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-ssao-enabled :class 'environment :bind "is_ssao_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-ssao-radius :class 'environment :bind "set_ssao_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (environment+get-ssao-radius :class 'environment :bind "get_ssao_radius" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-ssao-intensity :class 'environment :bind "set_ssao_intensity"
  :hash 373806689)
 :void (intensity float))

(defgmethod
 (environment+get-ssao-intensity :class 'environment :bind "get_ssao_intensity"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssao-power :class 'environment :bind "set_ssao_power" :hash
  373806689)
 :void (power float))

(defgmethod
 (environment+get-ssao-power :class 'environment :bind "get_ssao_power" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-ssao-detail :class 'environment :bind "set_ssao_detail" :hash
  373806689)
 :void (detail float))

(defgmethod
 (environment+get-ssao-detail :class 'environment :bind "get_ssao_detail" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-ssao-horizon :class 'environment :bind "set_ssao_horizon"
  :hash 373806689)
 :void (horizon float))

(defgmethod
 (environment+get-ssao-horizon :class 'environment :bind "get_ssao_horizon"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssao-sharpness :class 'environment :bind "set_ssao_sharpness"
  :hash 373806689)
 :void (sharpness float))

(defgmethod
 (environment+get-ssao-sharpness :class 'environment :bind "get_ssao_sharpness"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssao-direct-light-affect :class 'environment :bind
  "set_ssao_direct_light_affect" :hash 373806689)
 :void (amount float))

(defgmethod
 (environment+get-ssao-direct-light-affect :class 'environment :bind
  "get_ssao_direct_light_affect" :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssao-ao-channel-affect :class 'environment :bind
  "set_ssao_ao_channel_affect" :hash 373806689)
 :void (amount float))

(defgmethod
 (environment+get-ssao-ao-channel-affect :class 'environment :bind
  "get_ssao_ao_channel_affect" :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssil-enabled :class 'environment :bind "set_ssil_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-ssil-enabled :class 'environment :bind "is_ssil_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-ssil-radius :class 'environment :bind "set_ssil_radius" :hash
  373806689)
 :void (radius float))

(defgmethod
 (environment+get-ssil-radius :class 'environment :bind "get_ssil_radius" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-ssil-intensity :class 'environment :bind "set_ssil_intensity"
  :hash 373806689)
 :void (intensity float))

(defgmethod
 (environment+get-ssil-intensity :class 'environment :bind "get_ssil_intensity"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssil-sharpness :class 'environment :bind "set_ssil_sharpness"
  :hash 373806689)
 :void (sharpness float))

(defgmethod
 (environment+get-ssil-sharpness :class 'environment :bind "get_ssil_sharpness"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-ssil-normal-rejection :class 'environment :bind
  "set_ssil_normal_rejection" :hash 373806689)
 :void (normal-rejection float))

(defgmethod
 (environment+get-ssil-normal-rejection :class 'environment :bind
  "get_ssil_normal_rejection" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-enabled :class 'environment :bind "set_sdfgi_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-sdfgi-enabled :class 'environment :bind "is_sdfgi_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (environment+set-sdfgi-cascades :class 'environment :bind "set_sdfgi_cascades"
  :hash 1286410249)
 :void (amount int))

(defgmethod
 (environment+get-sdfgi-cascades :class 'environment :bind "get_sdfgi_cascades"
  :hash 3905245786)
 int)

(defgmethod
 (environment+set-sdfgi-min-cell-size :class 'environment :bind
  "set_sdfgi_min_cell_size" :hash 373806689)
 :void (size float))

(defgmethod
 (environment+get-sdfgi-min-cell-size :class 'environment :bind
  "get_sdfgi_min_cell_size" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-max-distance :class 'environment :bind
  "set_sdfgi_max_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (environment+get-sdfgi-max-distance :class 'environment :bind
  "get_sdfgi_max_distance" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-cascade0-distance :class 'environment :bind
  "set_sdfgi_cascade0_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (environment+get-sdfgi-cascade0-distance :class 'environment :bind
  "get_sdfgi_cascade0_distance" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-y-scale :class 'environment :bind "set_sdfgi_y_scale"
  :hash 3608608372)
 :void (scale environment+sdfgiyscale))

(defgmethod
 (environment+get-sdfgi-y-scale :class 'environment :bind "get_sdfgi_y_scale"
  :hash 2568002245)
 environment+sdfgiyscale)

(defgmethod
 (environment+set-sdfgi-use-occlusion :class 'environment :bind
  "set_sdfgi_use_occlusion" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (environment+is-sdfgi-using-occlusion :class 'environment :bind
  "is_sdfgi_using_occlusion" :hash 36873697)
 bool)

(defgmethod
 (environment+set-sdfgi-bounce-feedback :class 'environment :bind
  "set_sdfgi_bounce_feedback" :hash 373806689)
 :void (amount float))

(defgmethod
 (environment+get-sdfgi-bounce-feedback :class 'environment :bind
  "get_sdfgi_bounce_feedback" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-read-sky-light :class 'environment :bind
  "set_sdfgi_read_sky_light" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (environment+is-sdfgi-reading-sky-light :class 'environment :bind
  "is_sdfgi_reading_sky_light" :hash 36873697)
 bool)

(defgmethod
 (environment+set-sdfgi-energy :class 'environment :bind "set_sdfgi_energy"
  :hash 373806689)
 :void (amount float))

(defgmethod
 (environment+get-sdfgi-energy :class 'environment :bind "get_sdfgi_energy"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-normal-bias :class 'environment :bind
  "set_sdfgi_normal_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (environment+get-sdfgi-normal-bias :class 'environment :bind
  "get_sdfgi_normal_bias" :hash 1740695150)
 float)

(defgmethod
 (environment+set-sdfgi-probe-bias :class 'environment :bind
  "set_sdfgi_probe_bias" :hash 373806689)
 :void (bias float))

(defgmethod
 (environment+get-sdfgi-probe-bias :class 'environment :bind
  "get_sdfgi_probe_bias" :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-enabled :class 'environment :bind "set_glow_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-glow-enabled :class 'environment :bind "is_glow_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-glow-level :class 'environment :bind "set_glow_level" :hash
  1602489585)
 :void (idx int) (intensity float))

(defgmethod
 (environment+get-glow-level :class 'environment :bind "get_glow_level" :hash
  2339986948)
 float (idx int))

(defgmethod
 (environment+set-glow-normalized :class 'environment :bind
  "set_glow_normalized" :hash 2586408642)
 :void (normalize bool))

(defgmethod
 (environment+is-glow-normalized :class 'environment :bind "is_glow_normalized"
  :hash 36873697)
 bool)

(defgmethod
 (environment+set-glow-intensity :class 'environment :bind "set_glow_intensity"
  :hash 373806689)
 :void (intensity float))

(defgmethod
 (environment+get-glow-intensity :class 'environment :bind "get_glow_intensity"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-strength :class 'environment :bind "set_glow_strength"
  :hash 373806689)
 :void (strength float))

(defgmethod
 (environment+get-glow-strength :class 'environment :bind "get_glow_strength"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-mix :class 'environment :bind "set_glow_mix" :hash
  373806689)
 :void (mix float))

(defgmethod
 (environment+get-glow-mix :class 'environment :bind "get_glow_mix" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-glow-bloom :class 'environment :bind "set_glow_bloom" :hash
  373806689)
 :void (amount float))

(defgmethod
 (environment+get-glow-bloom :class 'environment :bind "get_glow_bloom" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-glow-blend-mode :class 'environment :bind
  "set_glow_blend_mode" :hash 2561587761)
 :void (mode environment+glow-blend-mode))

(defgmethod
 (environment+get-glow-blend-mode :class 'environment :bind
  "get_glow_blend_mode" :hash 1529667332)
 environment+glow-blend-mode)

(defgmethod
 (environment+set-glow-hdr-bleed-threshold :class 'environment :bind
  "set_glow_hdr_bleed_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (environment+get-glow-hdr-bleed-threshold :class 'environment :bind
  "get_glow_hdr_bleed_threshold" :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-hdr-bleed-scale :class 'environment :bind
  "set_glow_hdr_bleed_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (environment+get-glow-hdr-bleed-scale :class 'environment :bind
  "get_glow_hdr_bleed_scale" :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-hdr-luminance-cap :class 'environment :bind
  "set_glow_hdr_luminance_cap" :hash 373806689)
 :void (amount float))

(defgmethod
 (environment+get-glow-hdr-luminance-cap :class 'environment :bind
  "get_glow_hdr_luminance_cap" :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-map-strength :class 'environment :bind
  "set_glow_map_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (environment+get-glow-map-strength :class 'environment :bind
  "get_glow_map_strength" :hash 1740695150)
 float)

(defgmethod
 (environment+set-glow-map :class 'environment :bind "set_glow_map" :hash
  1790811099)
 :void (mode texture))

(defgmethod
 (environment+get-glow-map :class 'environment :bind "get_glow_map" :hash
  4037048985)
 texture)

(defgmethod
 (environment+set-fog-enabled :class 'environment :bind "set_fog_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-fog-enabled :class 'environment :bind "is_fog_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-fog-mode :class 'environment :bind "set_fog_mode" :hash
  3059806579)
 :void (mode environment+fog-mode))

(defgmethod
 (environment+get-fog-mode :class 'environment :bind "get_fog_mode" :hash
  2456062483)
 environment+fog-mode)

(defgmethod
 (environment+set-fog-light-color :class 'environment :bind
  "set_fog_light_color" :hash 2920490490)
 :void (light-color color))

(defgmethod
 (environment+get-fog-light-color :class 'environment :bind
  "get_fog_light_color" :hash 3444240500)
 color)

(defgmethod
 (environment+set-fog-light-energy :class 'environment :bind
  "set_fog_light_energy" :hash 373806689)
 :void (light-energy float))

(defgmethod
 (environment+get-fog-light-energy :class 'environment :bind
  "get_fog_light_energy" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-sun-scatter :class 'environment :bind
  "set_fog_sun_scatter" :hash 373806689)
 :void (sun-scatter float))

(defgmethod
 (environment+get-fog-sun-scatter :class 'environment :bind
  "get_fog_sun_scatter" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-density :class 'environment :bind "set_fog_density" :hash
  373806689)
 :void (density float))

(defgmethod
 (environment+get-fog-density :class 'environment :bind "get_fog_density" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-fog-height :class 'environment :bind "set_fog_height" :hash
  373806689)
 :void (height float))

(defgmethod
 (environment+get-fog-height :class 'environment :bind "get_fog_height" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-fog-height-density :class 'environment :bind
  "set_fog_height_density" :hash 373806689)
 :void (height-density float))

(defgmethod
 (environment+get-fog-height-density :class 'environment :bind
  "get_fog_height_density" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-aerial-perspective :class 'environment :bind
  "set_fog_aerial_perspective" :hash 373806689)
 :void (aerial-perspective float))

(defgmethod
 (environment+get-fog-aerial-perspective :class 'environment :bind
  "get_fog_aerial_perspective" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-sky-affect :class 'environment :bind "set_fog_sky_affect"
  :hash 373806689)
 :void (sky-affect float))

(defgmethod
 (environment+get-fog-sky-affect :class 'environment :bind "get_fog_sky_affect"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-depth-curve :class 'environment :bind
  "set_fog_depth_curve" :hash 373806689)
 :void (curve float))

(defgmethod
 (environment+get-fog-depth-curve :class 'environment :bind
  "get_fog_depth_curve" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-depth-begin :class 'environment :bind
  "set_fog_depth_begin" :hash 373806689)
 :void (begin float))

(defgmethod
 (environment+get-fog-depth-begin :class 'environment :bind
  "get_fog_depth_begin" :hash 1740695150)
 float)

(defgmethod
 (environment+set-fog-depth-end :class 'environment :bind "set_fog_depth_end"
  :hash 373806689)
 :void (end float))

(defgmethod
 (environment+get-fog-depth-end :class 'environment :bind "get_fog_depth_end"
  :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-enabled :class 'environment :bind
  "set_volumetric_fog_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-volumetric-fog-enabled :class 'environment :bind
  "is_volumetric_fog_enabled" :hash 36873697)
 bool)

(defgmethod
 (environment+set-volumetric-fog-emission :class 'environment :bind
  "set_volumetric_fog_emission" :hash 2920490490)
 :void (color color))

(defgmethod
 (environment+get-volumetric-fog-emission :class 'environment :bind
  "get_volumetric_fog_emission" :hash 3444240500)
 color)

(defgmethod
 (environment+set-volumetric-fog-albedo :class 'environment :bind
  "set_volumetric_fog_albedo" :hash 2920490490)
 :void (color color))

(defgmethod
 (environment+get-volumetric-fog-albedo :class 'environment :bind
  "get_volumetric_fog_albedo" :hash 3444240500)
 color)

(defgmethod
 (environment+set-volumetric-fog-density :class 'environment :bind
  "set_volumetric_fog_density" :hash 373806689)
 :void (density float))

(defgmethod
 (environment+get-volumetric-fog-density :class 'environment :bind
  "get_volumetric_fog_density" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-emission-energy :class 'environment :bind
  "set_volumetric_fog_emission_energy" :hash 373806689)
 :void (begin float))

(defgmethod
 (environment+get-volumetric-fog-emission-energy :class 'environment :bind
  "get_volumetric_fog_emission_energy" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-anisotropy :class 'environment :bind
  "set_volumetric_fog_anisotropy" :hash 373806689)
 :void (anisotropy float))

(defgmethod
 (environment+get-volumetric-fog-anisotropy :class 'environment :bind
  "get_volumetric_fog_anisotropy" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-length :class 'environment :bind
  "set_volumetric_fog_length" :hash 373806689)
 :void (length float))

(defgmethod
 (environment+get-volumetric-fog-length :class 'environment :bind
  "get_volumetric_fog_length" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-detail-spread :class 'environment :bind
  "set_volumetric_fog_detail_spread" :hash 373806689)
 :void (detail-spread float))

(defgmethod
 (environment+get-volumetric-fog-detail-spread :class 'environment :bind
  "get_volumetric_fog_detail_spread" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-gi-inject :class 'environment :bind
  "set_volumetric_fog_gi_inject" :hash 373806689)
 :void (gi-inject float))

(defgmethod
 (environment+get-volumetric-fog-gi-inject :class 'environment :bind
  "get_volumetric_fog_gi_inject" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-ambient-inject :class 'environment :bind
  "set_volumetric_fog_ambient_inject" :hash 373806689)
 :void (enabled float))

(defgmethod
 (environment+get-volumetric-fog-ambient-inject :class 'environment :bind
  "get_volumetric_fog_ambient_inject" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-sky-affect :class 'environment :bind
  "set_volumetric_fog_sky_affect" :hash 373806689)
 :void (sky-affect float))

(defgmethod
 (environment+get-volumetric-fog-sky-affect :class 'environment :bind
  "get_volumetric_fog_sky_affect" :hash 1740695150)
 float)

(defgmethod
 (environment+set-volumetric-fog-temporal-reprojection-enabled :class
  'environment :bind "set_volumetric_fog_temporal_reprojection_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-volumetric-fog-temporal-reprojection-enabled :class
  'environment :bind "is_volumetric_fog_temporal_reprojection_enabled" :hash
  36873697)
 bool)

(defgmethod
 (environment+set-volumetric-fog-temporal-reprojection-amount :class
  'environment :bind "set_volumetric_fog_temporal_reprojection_amount" :hash
  373806689)
 :void (temporal-reprojection-amount float))

(defgmethod
 (environment+get-volumetric-fog-temporal-reprojection-amount :class
  'environment :bind "get_volumetric_fog_temporal_reprojection_amount" :hash
  1740695150)
 float)

(defgmethod
 (environment+set-adjustment-enabled :class 'environment :bind
  "set_adjustment_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (environment+is-adjustment-enabled :class 'environment :bind
  "is_adjustment_enabled" :hash 36873697)
 bool)

(defgmethod
 (environment+set-adjustment-brightness :class 'environment :bind
  "set_adjustment_brightness" :hash 373806689)
 :void (brightness float))

(defgmethod
 (environment+get-adjustment-brightness :class 'environment :bind
  "get_adjustment_brightness" :hash 1740695150)
 float)

(defgmethod
 (environment+set-adjustment-contrast :class 'environment :bind
  "set_adjustment_contrast" :hash 373806689)
 :void (contrast float))

(defgmethod
 (environment+get-adjustment-contrast :class 'environment :bind
  "get_adjustment_contrast" :hash 1740695150)
 float)

(defgmethod
 (environment+set-adjustment-saturation :class 'environment :bind
  "set_adjustment_saturation" :hash 373806689)
 :void (saturation float))

(defgmethod
 (environment+get-adjustment-saturation :class 'environment :bind
  "get_adjustment_saturation" :hash 1740695150)
 float)

(defgmethod
 (environment+set-adjustment-color-correction :class 'environment :bind
  "set_adjustment_color_correction" :hash 1790811099)
 :void (color-correction texture))

(defgmethod
 (environment+get-adjustment-color-correction :class 'environment :bind
  "get_adjustment_color_correction" :hash 4037048985)
 texture)