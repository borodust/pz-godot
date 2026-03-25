(common-lisp:in-package :%godot)


(defgmethod
 (base-material-3d+set-albedo :class 'base-material-3d :bind "set_albedo" :hash
  2920490490)
 :void (albedo color))

(defgmethod
 (base-material-3d+get-albedo :class 'base-material-3d :bind "get_albedo" :hash
  3444240500)
 color)

(defgmethod
 (base-material-3d+set-transparency :class 'base-material-3d :bind
  "set_transparency" :hash 3435651667)
 :void (transparency base-material-3d+transparency))

(defgmethod
 (base-material-3d+get-transparency :class 'base-material-3d :bind
  "get_transparency" :hash 990903061)
 base-material-3d+transparency)

(defgmethod
 (base-material-3d+set-alpha-antialiasing :class 'base-material-3d :bind
  "set_alpha_antialiasing" :hash 3212649852)
 :void (alpha-aa base-material-3d+alpha-anti-aliasing))

(defgmethod
 (base-material-3d+get-alpha-antialiasing :class 'base-material-3d :bind
  "get_alpha_antialiasing" :hash 2889939400)
 base-material-3d+alpha-anti-aliasing)

(defgmethod
 (base-material-3d+set-alpha-antialiasing-edge :class 'base-material-3d :bind
  "set_alpha_antialiasing_edge" :hash 373806689)
 :void (edge float))

(defgmethod
 (base-material-3d+get-alpha-antialiasing-edge :class 'base-material-3d :bind
  "get_alpha_antialiasing_edge" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-shading-mode :class 'base-material-3d :bind
  "set_shading_mode" :hash 3368750322)
 :void (shading-mode base-material-3d+shading-mode))

(defgmethod
 (base-material-3d+get-shading-mode :class 'base-material-3d :bind
  "get_shading_mode" :hash 2132070559)
 base-material-3d+shading-mode)

(defgmethod
 (base-material-3d+set-specular :class 'base-material-3d :bind "set_specular"
  :hash 373806689)
 :void (specular float))

(defgmethod
 (base-material-3d+get-specular :class 'base-material-3d :bind "get_specular"
  :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-metallic :class 'base-material-3d :bind "set_metallic"
  :hash 373806689)
 :void (metallic float))

(defgmethod
 (base-material-3d+get-metallic :class 'base-material-3d :bind "get_metallic"
  :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-roughness :class 'base-material-3d :bind "set_roughness"
  :hash 373806689)
 :void (roughness float))

(defgmethod
 (base-material-3d+get-roughness :class 'base-material-3d :bind "get_roughness"
  :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-emission :class 'base-material-3d :bind "set_emission"
  :hash 2920490490)
 :void (emission color))

(defgmethod
 (base-material-3d+get-emission :class 'base-material-3d :bind "get_emission"
  :hash 3444240500)
 color)

(defgmethod
 (base-material-3d+set-emission-energy-multiplier :class 'base-material-3d
  :bind "set_emission_energy_multiplier" :hash 373806689)
 :void (emission-energy-multiplier float))

(defgmethod
 (base-material-3d+get-emission-energy-multiplier :class 'base-material-3d
  :bind "get_emission_energy_multiplier" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-emission-intensity :class 'base-material-3d :bind
  "set_emission_intensity" :hash 373806689)
 :void (emission-energy-multiplier float))

(defgmethod
 (base-material-3d+get-emission-intensity :class 'base-material-3d :bind
  "get_emission_intensity" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-normal-scale :class 'base-material-3d :bind
  "set_normal_scale" :hash 373806689)
 :void (normal-scale float))

(defgmethod
 (base-material-3d+get-normal-scale :class 'base-material-3d :bind
  "get_normal_scale" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-rim :class 'base-material-3d :bind "set_rim" :hash
  373806689)
 :void (rim float))

(defgmethod
 (base-material-3d+get-rim :class 'base-material-3d :bind "get_rim" :hash
  1740695150)
 float)

(defgmethod
 (base-material-3d+set-rim-tint :class 'base-material-3d :bind "set_rim_tint"
  :hash 373806689)
 :void (rim-tint float))

(defgmethod
 (base-material-3d+get-rim-tint :class 'base-material-3d :bind "get_rim_tint"
  :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-clearcoat :class 'base-material-3d :bind "set_clearcoat"
  :hash 373806689)
 :void (clearcoat float))

(defgmethod
 (base-material-3d+get-clearcoat :class 'base-material-3d :bind "get_clearcoat"
  :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-clearcoat-roughness :class 'base-material-3d :bind
  "set_clearcoat_roughness" :hash 373806689)
 :void (clearcoat-roughness float))

(defgmethod
 (base-material-3d+get-clearcoat-roughness :class 'base-material-3d :bind
  "get_clearcoat_roughness" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-anisotropy :class 'base-material-3d :bind
  "set_anisotropy" :hash 373806689)
 :void (anisotropy float))

(defgmethod
 (base-material-3d+get-anisotropy :class 'base-material-3d :bind
  "get_anisotropy" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-heightmap-scale :class 'base-material-3d :bind
  "set_heightmap_scale" :hash 373806689)
 :void (heightmap-scale float))

(defgmethod
 (base-material-3d+get-heightmap-scale :class 'base-material-3d :bind
  "get_heightmap_scale" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-subsurface-scattering-strength :class 'base-material-3d
  :bind "set_subsurface_scattering_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (base-material-3d+get-subsurface-scattering-strength :class 'base-material-3d
  :bind "get_subsurface_scattering_strength" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-transmittance-color :class 'base-material-3d :bind
  "set_transmittance_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (base-material-3d+get-transmittance-color :class 'base-material-3d :bind
  "get_transmittance_color" :hash 3444240500)
 color)

(defgmethod
 (base-material-3d+set-transmittance-depth :class 'base-material-3d :bind
  "set_transmittance_depth" :hash 373806689)
 :void (depth float))

(defgmethod
 (base-material-3d+get-transmittance-depth :class 'base-material-3d :bind
  "get_transmittance_depth" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-transmittance-boost :class 'base-material-3d :bind
  "set_transmittance_boost" :hash 373806689)
 :void (boost float))

(defgmethod
 (base-material-3d+get-transmittance-boost :class 'base-material-3d :bind
  "get_transmittance_boost" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-backlight :class 'base-material-3d :bind "set_backlight"
  :hash 2920490490)
 :void (backlight color))

(defgmethod
 (base-material-3d+get-backlight :class 'base-material-3d :bind "get_backlight"
  :hash 3444240500)
 color)

(defgmethod
 (base-material-3d+set-refraction :class 'base-material-3d :bind
  "set_refraction" :hash 373806689)
 :void (refraction float))

(defgmethod
 (base-material-3d+get-refraction :class 'base-material-3d :bind
  "get_refraction" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-point-size :class 'base-material-3d :bind
  "set_point_size" :hash 373806689)
 :void (point-size float))

(defgmethod
 (base-material-3d+get-point-size :class 'base-material-3d :bind
  "get_point_size" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-detail-uv :class 'base-material-3d :bind "set_detail_uv"
  :hash 456801921)
 :void (detail-uv base-material-3d+detail-uv))

(defgmethod
 (base-material-3d+get-detail-uv :class 'base-material-3d :bind "get_detail_uv"
  :hash 2306920512)
 base-material-3d+detail-uv)

(defgmethod
 (base-material-3d+set-blend-mode :class 'base-material-3d :bind
  "set_blend_mode" :hash 2830186259)
 :void (blend-mode base-material-3d+blend-mode))

(defgmethod
 (base-material-3d+get-blend-mode :class 'base-material-3d :bind
  "get_blend_mode" :hash 4022690962)
 base-material-3d+blend-mode)

(defgmethod
 (base-material-3d+set-depth-draw-mode :class 'base-material-3d :bind
  "set_depth_draw_mode" :hash 1456584748)
 :void (depth-draw-mode base-material-3d+depth-draw-mode))

(defgmethod
 (base-material-3d+get-depth-draw-mode :class 'base-material-3d :bind
  "get_depth_draw_mode" :hash 2578197639)
 base-material-3d+depth-draw-mode)

(defgmethod
 (base-material-3d+set-depth-test :class 'base-material-3d :bind
  "set_depth_test" :hash 3918692338)
 :void (depth-test base-material-3d+depth-test))

(defgmethod
 (base-material-3d+get-depth-test :class 'base-material-3d :bind
  "get_depth_test" :hash 3434785811)
 base-material-3d+depth-test)

(defgmethod
 (base-material-3d+set-cull-mode :class 'base-material-3d :bind "set_cull_mode"
  :hash 2338909218)
 :void (cull-mode base-material-3d+cull-mode))

(defgmethod
 (base-material-3d+get-cull-mode :class 'base-material-3d :bind "get_cull_mode"
  :hash 1941499586)
 base-material-3d+cull-mode)

(defgmethod
 (base-material-3d+set-diffuse-mode :class 'base-material-3d :bind
  "set_diffuse_mode" :hash 1045299638)
 :void (diffuse-mode base-material-3d+diffuse-mode))

(defgmethod
 (base-material-3d+get-diffuse-mode :class 'base-material-3d :bind
  "get_diffuse_mode" :hash 3973617136)
 base-material-3d+diffuse-mode)

(defgmethod
 (base-material-3d+set-specular-mode :class 'base-material-3d :bind
  "set_specular_mode" :hash 584737147)
 :void (specular-mode base-material-3d+specular-mode))

(defgmethod
 (base-material-3d+get-specular-mode :class 'base-material-3d :bind
  "get_specular_mode" :hash 2569953298)
 base-material-3d+specular-mode)

(defgmethod
 (base-material-3d+set-flag :class 'base-material-3d :bind "set_flag" :hash
  3070159527)
 :void (flag base-material-3d+flags) (enable bool))

(defgmethod
 (base-material-3d+get-flag :class 'base-material-3d :bind "get_flag" :hash
  1286410065)
 bool (flag base-material-3d+flags))

(defgmethod
 (base-material-3d+set-texture-filter :class 'base-material-3d :bind
  "set_texture_filter" :hash 22904437)
 :void (mode base-material-3d+texture-filter))

(defgmethod
 (base-material-3d+get-texture-filter :class 'base-material-3d :bind
  "get_texture_filter" :hash 3289213076)
 base-material-3d+texture-filter)

(defgmethod
 (base-material-3d+set-feature :class 'base-material-3d :bind "set_feature"
  :hash 2819288693)
 :void (feature base-material-3d+feature) (enable bool))

(defgmethod
 (base-material-3d+get-feature :class 'base-material-3d :bind "get_feature"
  :hash 1965241794)
 bool (feature base-material-3d+feature))

(defgmethod
 (base-material-3d+set-texture :class 'base-material-3d :bind "set_texture"
  :hash 464208135)
 :void (param base-material-3d+texture-param) (texture texture-2d))

(defgmethod
 (base-material-3d+get-texture :class 'base-material-3d :bind "get_texture"
  :hash 329605813)
 texture-2d (param base-material-3d+texture-param))

(defgmethod
 (base-material-3d+set-detail-blend-mode :class 'base-material-3d :bind
  "set_detail_blend_mode" :hash 2830186259)
 :void (detail-blend-mode base-material-3d+blend-mode))

(defgmethod
 (base-material-3d+get-detail-blend-mode :class 'base-material-3d :bind
  "get_detail_blend_mode" :hash 4022690962)
 base-material-3d+blend-mode)

(defgmethod
 (base-material-3d+set-uv1-scale :class 'base-material-3d :bind "set_uv1_scale"
  :hash 3460891852)
 :void (scale vector-3))

(defgmethod
 (base-material-3d+get-uv1-scale :class 'base-material-3d :bind "get_uv1_scale"
  :hash 3360562783)
 vector-3)

(defgmethod
 (base-material-3d+set-uv1-offset :class 'base-material-3d :bind
  "set_uv1_offset" :hash 3460891852)
 :void (offset vector-3))

(defgmethod
 (base-material-3d+get-uv1-offset :class 'base-material-3d :bind
  "get_uv1_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (base-material-3d+set-uv1-triplanar-blend-sharpness :class 'base-material-3d
  :bind "set_uv1_triplanar_blend_sharpness" :hash 373806689)
 :void (sharpness float))

(defgmethod
 (base-material-3d+get-uv1-triplanar-blend-sharpness :class 'base-material-3d
  :bind "get_uv1_triplanar_blend_sharpness" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-uv2-scale :class 'base-material-3d :bind "set_uv2_scale"
  :hash 3460891852)
 :void (scale vector-3))

(defgmethod
 (base-material-3d+get-uv2-scale :class 'base-material-3d :bind "get_uv2_scale"
  :hash 3360562783)
 vector-3)

(defgmethod
 (base-material-3d+set-uv2-offset :class 'base-material-3d :bind
  "set_uv2_offset" :hash 3460891852)
 :void (offset vector-3))

(defgmethod
 (base-material-3d+get-uv2-offset :class 'base-material-3d :bind
  "get_uv2_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (base-material-3d+set-uv2-triplanar-blend-sharpness :class 'base-material-3d
  :bind "set_uv2_triplanar_blend_sharpness" :hash 373806689)
 :void (sharpness float))

(defgmethod
 (base-material-3d+get-uv2-triplanar-blend-sharpness :class 'base-material-3d
  :bind "get_uv2_triplanar_blend_sharpness" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-billboard-mode :class 'base-material-3d :bind
  "set_billboard_mode" :hash 4202036497)
 :void (mode base-material-3d+billboard-mode))

(defgmethod
 (base-material-3d+get-billboard-mode :class 'base-material-3d :bind
  "get_billboard_mode" :hash 1283840139)
 base-material-3d+billboard-mode)

(defgmethod
 (base-material-3d+set-particles-anim-h-frames :class 'base-material-3d :bind
  "set_particles_anim_h_frames" :hash 1286410249)
 :void (frames int))

(defgmethod
 (base-material-3d+get-particles-anim-h-frames :class 'base-material-3d :bind
  "get_particles_anim_h_frames" :hash 3905245786)
 int)

(defgmethod
 (base-material-3d+set-particles-anim-v-frames :class 'base-material-3d :bind
  "set_particles_anim_v_frames" :hash 1286410249)
 :void (frames int))

(defgmethod
 (base-material-3d+get-particles-anim-v-frames :class 'base-material-3d :bind
  "get_particles_anim_v_frames" :hash 3905245786)
 int)

(defgmethod
 (base-material-3d+set-particles-anim-loop :class 'base-material-3d :bind
  "set_particles_anim_loop" :hash 2586408642)
 :void (loop bool))

(defgmethod
 (base-material-3d+get-particles-anim-loop :class 'base-material-3d :bind
  "get_particles_anim_loop" :hash 36873697)
 bool)

(defgmethod
 (base-material-3d+set-heightmap-deep-parallax :class 'base-material-3d :bind
  "set_heightmap_deep_parallax" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (base-material-3d+is-heightmap-deep-parallax-enabled :class 'base-material-3d
  :bind "is_heightmap_deep_parallax_enabled" :hash 36873697)
 bool)

(defgmethod
 (base-material-3d+set-heightmap-deep-parallax-min-layers :class
  'base-material-3d :bind "set_heightmap_deep_parallax_min_layers" :hash
  1286410249)
 :void (layer int))

(defgmethod
 (base-material-3d+get-heightmap-deep-parallax-min-layers :class
  'base-material-3d :bind "get_heightmap_deep_parallax_min_layers" :hash
  3905245786)
 int)

(defgmethod
 (base-material-3d+set-heightmap-deep-parallax-max-layers :class
  'base-material-3d :bind "set_heightmap_deep_parallax_max_layers" :hash
  1286410249)
 :void (layer int))

(defgmethod
 (base-material-3d+get-heightmap-deep-parallax-max-layers :class
  'base-material-3d :bind "get_heightmap_deep_parallax_max_layers" :hash
  3905245786)
 int)

(defgmethod
 (base-material-3d+set-heightmap-deep-parallax-flip-tangent :class
  'base-material-3d :bind "set_heightmap_deep_parallax_flip_tangent" :hash
  2586408642)
 :void (flip bool))

(defgmethod
 (base-material-3d+get-heightmap-deep-parallax-flip-tangent :class
  'base-material-3d :bind "get_heightmap_deep_parallax_flip_tangent" :hash
  36873697)
 bool)

(defgmethod
 (base-material-3d+set-heightmap-deep-parallax-flip-binormal :class
  'base-material-3d :bind "set_heightmap_deep_parallax_flip_binormal" :hash
  2586408642)
 :void (flip bool))

(defgmethod
 (base-material-3d+get-heightmap-deep-parallax-flip-binormal :class
  'base-material-3d :bind "get_heightmap_deep_parallax_flip_binormal" :hash
  36873697)
 bool)

(defgmethod
 (base-material-3d+set-grow :class 'base-material-3d :bind "set_grow" :hash
  373806689)
 :void (amount float))

(defgmethod
 (base-material-3d+get-grow :class 'base-material-3d :bind "get_grow" :hash
  1740695150)
 float)

(defgmethod
 (base-material-3d+set-emission-operator :class 'base-material-3d :bind
  "set_emission_operator" :hash 3825128922)
 :void (operator base-material-3d+emission-operator))

(defgmethod
 (base-material-3d+get-emission-operator :class 'base-material-3d :bind
  "get_emission_operator" :hash 974205018)
 base-material-3d+emission-operator)

(defgmethod
 (base-material-3d+set-ao-light-affect :class 'base-material-3d :bind
  "set_ao_light_affect" :hash 373806689)
 :void (amount float))

(defgmethod
 (base-material-3d+get-ao-light-affect :class 'base-material-3d :bind
  "get_ao_light_affect" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-alpha-scissor-threshold :class 'base-material-3d :bind
  "set_alpha_scissor_threshold" :hash 373806689)
 :void (threshold float))

(defgmethod
 (base-material-3d+get-alpha-scissor-threshold :class 'base-material-3d :bind
  "get_alpha_scissor_threshold" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-alpha-hash-scale :class 'base-material-3d :bind
  "set_alpha_hash_scale" :hash 373806689)
 :void (threshold float))

(defgmethod
 (base-material-3d+get-alpha-hash-scale :class 'base-material-3d :bind
  "get_alpha_hash_scale" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-grow-enabled :class 'base-material-3d :bind
  "set_grow_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (base-material-3d+is-grow-enabled :class 'base-material-3d :bind
  "is_grow_enabled" :hash 36873697)
 bool)

(defgmethod
 (base-material-3d+set-metallic-texture-channel :class 'base-material-3d :bind
  "set_metallic_texture_channel" :hash 744167988)
 :void (channel base-material-3d+texture-channel))

(defgmethod
 (base-material-3d+get-metallic-texture-channel :class 'base-material-3d :bind
  "get_metallic_texture_channel" :hash 568133867)
 base-material-3d+texture-channel)

(defgmethod
 (base-material-3d+set-roughness-texture-channel :class 'base-material-3d :bind
  "set_roughness_texture_channel" :hash 744167988)
 :void (channel base-material-3d+texture-channel))

(defgmethod
 (base-material-3d+get-roughness-texture-channel :class 'base-material-3d :bind
  "get_roughness_texture_channel" :hash 568133867)
 base-material-3d+texture-channel)

(defgmethod
 (base-material-3d+set-ao-texture-channel :class 'base-material-3d :bind
  "set_ao_texture_channel" :hash 744167988)
 :void (channel base-material-3d+texture-channel))

(defgmethod
 (base-material-3d+get-ao-texture-channel :class 'base-material-3d :bind
  "get_ao_texture_channel" :hash 568133867)
 base-material-3d+texture-channel)

(defgmethod
 (base-material-3d+set-refraction-texture-channel :class 'base-material-3d
  :bind "set_refraction_texture_channel" :hash 744167988)
 :void (channel base-material-3d+texture-channel))

(defgmethod
 (base-material-3d+get-refraction-texture-channel :class 'base-material-3d
  :bind "get_refraction_texture_channel" :hash 568133867)
 base-material-3d+texture-channel)

(defgmethod
 (base-material-3d+set-proximity-fade-enabled :class 'base-material-3d :bind
  "set_proximity_fade_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (base-material-3d+is-proximity-fade-enabled :class 'base-material-3d :bind
  "is_proximity_fade_enabled" :hash 36873697)
 bool)

(defgmethod
 (base-material-3d+set-proximity-fade-distance :class 'base-material-3d :bind
  "set_proximity_fade_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (base-material-3d+get-proximity-fade-distance :class 'base-material-3d :bind
  "get_proximity_fade_distance" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-msdf-pixel-range :class 'base-material-3d :bind
  "set_msdf_pixel_range" :hash 373806689)
 :void (range float))

(defgmethod
 (base-material-3d+get-msdf-pixel-range :class 'base-material-3d :bind
  "get_msdf_pixel_range" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-msdf-outline-size :class 'base-material-3d :bind
  "set_msdf_outline_size" :hash 373806689)
 :void (size float))

(defgmethod
 (base-material-3d+get-msdf-outline-size :class 'base-material-3d :bind
  "get_msdf_outline_size" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-distance-fade :class 'base-material-3d :bind
  "set_distance_fade" :hash 1379478617)
 :void (mode base-material-3d+distance-fade-mode))

(defgmethod
 (base-material-3d+get-distance-fade :class 'base-material-3d :bind
  "get_distance_fade" :hash 2694575734)
 base-material-3d+distance-fade-mode)

(defgmethod
 (base-material-3d+set-distance-fade-max-distance :class 'base-material-3d
  :bind "set_distance_fade_max_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (base-material-3d+get-distance-fade-max-distance :class 'base-material-3d
  :bind "get_distance_fade_max_distance" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-distance-fade-min-distance :class 'base-material-3d
  :bind "set_distance_fade_min_distance" :hash 373806689)
 :void (distance float))

(defgmethod
 (base-material-3d+get-distance-fade-min-distance :class 'base-material-3d
  :bind "get_distance_fade_min_distance" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-z-clip-scale :class 'base-material-3d :bind
  "set_z_clip_scale" :hash 373806689)
 :void (scale float))

(defgmethod
 (base-material-3d+get-z-clip-scale :class 'base-material-3d :bind
  "get_z_clip_scale" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-fov-override :class 'base-material-3d :bind
  "set_fov_override" :hash 373806689)
 :void (scale float))

(defgmethod
 (base-material-3d+get-fov-override :class 'base-material-3d :bind
  "get_fov_override" :hash 1740695150)
 float)

(defgmethod
 (base-material-3d+set-stencil-mode :class 'base-material-3d :bind
  "set_stencil_mode" :hash 2272367200)
 :void (stencil-mode base-material-3d+stencil-mode))

(defgmethod
 (base-material-3d+get-stencil-mode :class 'base-material-3d :bind
  "get_stencil_mode" :hash 2908443456)
 base-material-3d+stencil-mode)

(defgmethod
 (base-material-3d+set-stencil-flags :class 'base-material-3d :bind
  "set_stencil_flags" :hash 1286410249)
 :void (stencil-flags int))

(defgmethod
 (base-material-3d+get-stencil-flags :class 'base-material-3d :bind
  "get_stencil_flags" :hash 3905245786)
 int)

(defgmethod
 (base-material-3d+set-stencil-compare :class 'base-material-3d :bind
  "set_stencil_compare" :hash 3741726481)
 :void (stencil-compare base-material-3d+stencil-compare))

(defgmethod
 (base-material-3d+get-stencil-compare :class 'base-material-3d :bind
  "get_stencil_compare" :hash 2824600492)
 base-material-3d+stencil-compare)

(defgmethod
 (base-material-3d+set-stencil-reference :class 'base-material-3d :bind
  "set_stencil_reference" :hash 1286410249)
 :void (stencil-reference int))

(defgmethod
 (base-material-3d+get-stencil-reference :class 'base-material-3d :bind
  "get_stencil_reference" :hash 3905245786)
 int)

(defgmethod
 (base-material-3d+set-stencil-effect-color :class 'base-material-3d :bind
  "set_stencil_effect_color" :hash 2920490490)
 :void (stencil-color color))

(defgmethod
 (base-material-3d+get-stencil-effect-color :class 'base-material-3d :bind
  "get_stencil_effect_color" :hash 3444240500)
 color)

(defgmethod
 (base-material-3d+set-stencil-effect-outline-thickness :class
  'base-material-3d :bind "set_stencil_effect_outline_thickness" :hash
  373806689)
 :void (stencil-outline-thickness float))

(defgmethod
 (base-material-3d+get-stencil-effect-outline-thickness :class
  'base-material-3d :bind "get_stencil_effect_outline_thickness" :hash
  1740695150)
 float)