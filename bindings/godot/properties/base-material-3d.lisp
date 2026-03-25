(common-lisp:in-package :%godot)


(defgproperty base-material-3d+transparency 'base-material-3d :get
 'base-material-3d+get-transparency :set 'base-material-3d+set-transparency)

(defgproperty base-material-3d+alpha-scissor-threshold 'base-material-3d :get
 'base-material-3d+get-alpha-scissor-threshold :set
 'base-material-3d+set-alpha-scissor-threshold)

(defgproperty base-material-3d+alpha-hash-scale 'base-material-3d :get
 'base-material-3d+get-alpha-hash-scale :set
 'base-material-3d+set-alpha-hash-scale)

(defgproperty base-material-3d+alpha-antialiasing-mode 'base-material-3d :get
 'base-material-3d+get-alpha-antialiasing :set
 'base-material-3d+set-alpha-antialiasing)

(defgproperty base-material-3d+alpha-antialiasing-edge 'base-material-3d :get
 'base-material-3d+get-alpha-antialiasing-edge :set
 'base-material-3d+set-alpha-antialiasing-edge)

(defgproperty base-material-3d+blend-mode 'base-material-3d :get
 'base-material-3d+get-blend-mode :set 'base-material-3d+set-blend-mode)

(defgproperty base-material-3d+cull-mode 'base-material-3d :get
 'base-material-3d+get-cull-mode :set 'base-material-3d+set-cull-mode)

(defgproperty base-material-3d+depth-draw-mode 'base-material-3d :get
 'base-material-3d+get-depth-draw-mode :set
 'base-material-3d+set-depth-draw-mode)

(defgproperty base-material-3d+no-depth-test 'base-material-3d :index 0 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+depth-test 'base-material-3d :get
 'base-material-3d+get-depth-test :set 'base-material-3d+set-depth-test)

(defgproperty base-material-3d+shading-mode 'base-material-3d :get
 'base-material-3d+get-shading-mode :set 'base-material-3d+set-shading-mode)

(defgproperty base-material-3d+diffuse-mode 'base-material-3d :get
 'base-material-3d+get-diffuse-mode :set 'base-material-3d+set-diffuse-mode)

(defgproperty base-material-3d+specular-mode 'base-material-3d :get
 'base-material-3d+get-specular-mode :set 'base-material-3d+set-specular-mode)

(defgproperty base-material-3d+disable-ambient-light 'base-material-3d :index
 14 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+disable-fog 'base-material-3d :index 21 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+disable-specular-occlusion 'base-material-3d
 :index 22 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+vertex-color-use-as-albedo 'base-material-3d
 :index 1 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+vertex-color-is-srgb 'base-material-3d :index 2
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+albedo-color 'base-material-3d :get
 'base-material-3d+get-albedo :set 'base-material-3d+set-albedo)

(defgproperty base-material-3d+albedo-texture 'base-material-3d :index 0 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+albedo-texture-force-srgb 'base-material-3d
 :index 12 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+albedo-texture-msdf 'base-material-3d :index 20
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+orm-texture 'base-material-3d :index 17 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+metallic 'base-material-3d :get
 'base-material-3d+get-metallic :set 'base-material-3d+set-metallic)

(defgproperty base-material-3d+metallic-specular 'base-material-3d :get
 'base-material-3d+get-specular :set 'base-material-3d+set-specular)

(defgproperty base-material-3d+metallic-texture 'base-material-3d :index 1 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+metallic-texture-channel 'base-material-3d :get
 'base-material-3d+get-metallic-texture-channel :set
 'base-material-3d+set-metallic-texture-channel)

(defgproperty base-material-3d+roughness 'base-material-3d :get
 'base-material-3d+get-roughness :set 'base-material-3d+set-roughness)

(defgproperty base-material-3d+roughness-texture 'base-material-3d :index 2
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+roughness-texture-channel 'base-material-3d :get
 'base-material-3d+get-roughness-texture-channel :set
 'base-material-3d+set-roughness-texture-channel)

(defgproperty base-material-3d+emission-enabled 'base-material-3d :index 0 :get
 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+emission 'base-material-3d :get
 'base-material-3d+get-emission :set 'base-material-3d+set-emission)

(defgproperty base-material-3d+emission-energy-multiplier 'base-material-3d
 :get 'base-material-3d+get-emission-energy-multiplier :set
 'base-material-3d+set-emission-energy-multiplier)

(defgproperty base-material-3d+emission-intensity 'base-material-3d :get
 'base-material-3d+get-emission-intensity :set
 'base-material-3d+set-emission-intensity)

(defgproperty base-material-3d+emission-operator 'base-material-3d :get
 'base-material-3d+get-emission-operator :set
 'base-material-3d+set-emission-operator)

(defgproperty base-material-3d+emission-on-uv2 'base-material-3d :index 11 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+emission-texture 'base-material-3d :index 3 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+normal-enabled 'base-material-3d :index 1 :get
 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+normal-scale 'base-material-3d :get
 'base-material-3d+get-normal-scale :set 'base-material-3d+set-normal-scale)

(defgproperty base-material-3d+normal-texture 'base-material-3d :index 4 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+bent-normal-enabled 'base-material-3d :index 12
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+bent-normal-texture 'base-material-3d :index 18
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+rim-enabled 'base-material-3d :index 2 :get
 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+rim 'base-material-3d :get
 'base-material-3d+get-rim :set 'base-material-3d+set-rim)

(defgproperty base-material-3d+rim-tint 'base-material-3d :get
 'base-material-3d+get-rim-tint :set 'base-material-3d+set-rim-tint)

(defgproperty base-material-3d+rim-texture 'base-material-3d :index 5 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+clearcoat-enabled 'base-material-3d :index 3
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+clearcoat 'base-material-3d :get
 'base-material-3d+get-clearcoat :set 'base-material-3d+set-clearcoat)

(defgproperty base-material-3d+clearcoat-roughness 'base-material-3d :get
 'base-material-3d+get-clearcoat-roughness :set
 'base-material-3d+set-clearcoat-roughness)

(defgproperty base-material-3d+clearcoat-texture 'base-material-3d :index 6
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+anisotropy-enabled 'base-material-3d :index 4
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+anisotropy 'base-material-3d :get
 'base-material-3d+get-anisotropy :set 'base-material-3d+set-anisotropy)

(defgproperty base-material-3d+anisotropy-flowmap 'base-material-3d :index 7
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+ao-enabled 'base-material-3d :index 5 :get
 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+ao-light-affect 'base-material-3d :get
 'base-material-3d+get-ao-light-affect :set
 'base-material-3d+set-ao-light-affect)

(defgproperty base-material-3d+ao-texture 'base-material-3d :index 8 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+ao-on-uv2 'base-material-3d :index 10 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+ao-texture-channel 'base-material-3d :get
 'base-material-3d+get-ao-texture-channel :set
 'base-material-3d+set-ao-texture-channel)

(defgproperty base-material-3d+heightmap-enabled 'base-material-3d :index 6
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+heightmap-scale 'base-material-3d :get
 'base-material-3d+get-heightmap-scale :set
 'base-material-3d+set-heightmap-scale)

(defgproperty base-material-3d+heightmap-deep-parallax 'base-material-3d :get
 'base-material-3d+is-heightmap-deep-parallax-enabled :set
 'base-material-3d+set-heightmap-deep-parallax)

(defgproperty base-material-3d+heightmap-min-layers 'base-material-3d :get
 'base-material-3d+get-heightmap-deep-parallax-min-layers :set
 'base-material-3d+set-heightmap-deep-parallax-min-layers)

(defgproperty base-material-3d+heightmap-max-layers 'base-material-3d :get
 'base-material-3d+get-heightmap-deep-parallax-max-layers :set
 'base-material-3d+set-heightmap-deep-parallax-max-layers)

(defgproperty base-material-3d+heightmap-flip-tangent 'base-material-3d :get
 'base-material-3d+get-heightmap-deep-parallax-flip-tangent :set
 'base-material-3d+set-heightmap-deep-parallax-flip-tangent)

(defgproperty base-material-3d+heightmap-flip-binormal 'base-material-3d :get
 'base-material-3d+get-heightmap-deep-parallax-flip-binormal :set
 'base-material-3d+set-heightmap-deep-parallax-flip-binormal)

(defgproperty base-material-3d+heightmap-texture 'base-material-3d :index 9
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+heightmap-flip-texture 'base-material-3d :index
 17 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+subsurf-scatter-enabled 'base-material-3d :index
 7 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+subsurf-scatter-strength 'base-material-3d :get
 'base-material-3d+get-subsurface-scattering-strength :set
 'base-material-3d+set-subsurface-scattering-strength)

(defgproperty base-material-3d+subsurf-scatter-skin-mode 'base-material-3d
 :index 18 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+subsurf-scatter-texture 'base-material-3d :index
 10 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+subsurf-scatter-transmittance-enabled
 'base-material-3d :index 8 :get 'base-material-3d+get-feature :set
 'base-material-3d+set-feature)

(defgproperty base-material-3d+subsurf-scatter-transmittance-color
 'base-material-3d :get 'base-material-3d+get-transmittance-color :set
 'base-material-3d+set-transmittance-color)

(defgproperty base-material-3d+subsurf-scatter-transmittance-texture
 'base-material-3d :index 11 :get 'base-material-3d+get-texture :set
 'base-material-3d+set-texture)

(defgproperty base-material-3d+subsurf-scatter-transmittance-depth
 'base-material-3d :get 'base-material-3d+get-transmittance-depth :set
 'base-material-3d+set-transmittance-depth)

(defgproperty base-material-3d+subsurf-scatter-transmittance-boost
 'base-material-3d :get 'base-material-3d+get-transmittance-boost :set
 'base-material-3d+set-transmittance-boost)

(defgproperty base-material-3d+backlight-enabled 'base-material-3d :index 9
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+backlight 'base-material-3d :get
 'base-material-3d+get-backlight :set 'base-material-3d+set-backlight)

(defgproperty base-material-3d+backlight-texture 'base-material-3d :index 12
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+refraction-enabled 'base-material-3d :index 10
 :get 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+refraction-scale 'base-material-3d :get
 'base-material-3d+get-refraction :set 'base-material-3d+set-refraction)

(defgproperty base-material-3d+refraction-texture 'base-material-3d :index 13
 :get 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+refraction-texture-channel 'base-material-3d
 :get 'base-material-3d+get-refraction-texture-channel :set
 'base-material-3d+set-refraction-texture-channel)

(defgproperty base-material-3d+detail-enabled 'base-material-3d :index 11 :get
 'base-material-3d+get-feature :set 'base-material-3d+set-feature)

(defgproperty base-material-3d+detail-mask 'base-material-3d :index 14 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+detail-blend-mode 'base-material-3d :get
 'base-material-3d+get-detail-blend-mode :set
 'base-material-3d+set-detail-blend-mode)

(defgproperty base-material-3d+detail-uv-layer 'base-material-3d :get
 'base-material-3d+get-detail-uv :set 'base-material-3d+set-detail-uv)

(defgproperty base-material-3d+detail-albedo 'base-material-3d :index 15 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+detail-normal 'base-material-3d :index 16 :get
 'base-material-3d+get-texture :set 'base-material-3d+set-texture)

(defgproperty base-material-3d+uv1-scale 'base-material-3d :get
 'base-material-3d+get-uv1-scale :set 'base-material-3d+set-uv1-scale)

(defgproperty base-material-3d+uv1-offset 'base-material-3d :get
 'base-material-3d+get-uv1-offset :set 'base-material-3d+set-uv1-offset)

(defgproperty base-material-3d+uv1-triplanar 'base-material-3d :index 6 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+uv1-triplanar-sharpness 'base-material-3d :get
 'base-material-3d+get-uv1-triplanar-blend-sharpness :set
 'base-material-3d+set-uv1-triplanar-blend-sharpness)

(defgproperty base-material-3d+uv1-world-triplanar 'base-material-3d :index 8
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+uv2-scale 'base-material-3d :get
 'base-material-3d+get-uv2-scale :set 'base-material-3d+set-uv2-scale)

(defgproperty base-material-3d+uv2-offset 'base-material-3d :get
 'base-material-3d+get-uv2-offset :set 'base-material-3d+set-uv2-offset)

(defgproperty base-material-3d+uv2-triplanar 'base-material-3d :index 7 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+uv2-triplanar-sharpness 'base-material-3d :get
 'base-material-3d+get-uv2-triplanar-blend-sharpness :set
 'base-material-3d+set-uv2-triplanar-blend-sharpness)

(defgproperty base-material-3d+uv2-world-triplanar 'base-material-3d :index 9
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+texture-filter 'base-material-3d :get
 'base-material-3d+get-texture-filter :set 'base-material-3d+set-texture-filter)

(defgproperty base-material-3d+texture-repeat 'base-material-3d :index 16 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+disable-receive-shadows 'base-material-3d :index
 13 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+shadow-to-opacity 'base-material-3d :index 15
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+billboard-mode 'base-material-3d :get
 'base-material-3d+get-billboard-mode :set 'base-material-3d+set-billboard-mode)

(defgproperty base-material-3d+billboard-keep-scale 'base-material-3d :index 5
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+particles-anim-h-frames 'base-material-3d :get
 'base-material-3d+get-particles-anim-h-frames :set
 'base-material-3d+set-particles-anim-h-frames)

(defgproperty base-material-3d+particles-anim-v-frames 'base-material-3d :get
 'base-material-3d+get-particles-anim-v-frames :set
 'base-material-3d+set-particles-anim-v-frames)

(defgproperty base-material-3d+particles-anim-loop 'base-material-3d :get
 'base-material-3d+get-particles-anim-loop :set
 'base-material-3d+set-particles-anim-loop)

(defgproperty base-material-3d+grow 'base-material-3d :get
 'base-material-3d+is-grow-enabled :set 'base-material-3d+set-grow-enabled)

(defgproperty base-material-3d+grow-amount 'base-material-3d :get
 'base-material-3d+get-grow :set 'base-material-3d+set-grow)

(defgproperty base-material-3d+fixed-size 'base-material-3d :index 4 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+use-point-size 'base-material-3d :index 3 :get
 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+point-size 'base-material-3d :get
 'base-material-3d+get-point-size :set 'base-material-3d+set-point-size)

(defgproperty base-material-3d+use-particle-trails 'base-material-3d :index 19
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+use-z-clip-scale 'base-material-3d :index 23
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+z-clip-scale 'base-material-3d :get
 'base-material-3d+get-z-clip-scale :set 'base-material-3d+set-z-clip-scale)

(defgproperty base-material-3d+use-fov-override 'base-material-3d :index 24
 :get 'base-material-3d+get-flag :set 'base-material-3d+set-flag)

(defgproperty base-material-3d+fov-override 'base-material-3d :get
 'base-material-3d+get-fov-override :set 'base-material-3d+set-fov-override)

(defgproperty base-material-3d+proximity-fade-enabled 'base-material-3d :get
 'base-material-3d+is-proximity-fade-enabled :set
 'base-material-3d+set-proximity-fade-enabled)

(defgproperty base-material-3d+proximity-fade-distance 'base-material-3d :get
 'base-material-3d+get-proximity-fade-distance :set
 'base-material-3d+set-proximity-fade-distance)

(defgproperty base-material-3d+msdf-pixel-range 'base-material-3d :get
 'base-material-3d+get-msdf-pixel-range :set
 'base-material-3d+set-msdf-pixel-range)

(defgproperty base-material-3d+msdf-outline-size 'base-material-3d :get
 'base-material-3d+get-msdf-outline-size :set
 'base-material-3d+set-msdf-outline-size)

(defgproperty base-material-3d+distance-fade-mode 'base-material-3d :get
 'base-material-3d+get-distance-fade :set 'base-material-3d+set-distance-fade)

(defgproperty base-material-3d+distance-fade-min-distance 'base-material-3d
 :get 'base-material-3d+get-distance-fade-min-distance :set
 'base-material-3d+set-distance-fade-min-distance)

(defgproperty base-material-3d+distance-fade-max-distance 'base-material-3d
 :get 'base-material-3d+get-distance-fade-max-distance :set
 'base-material-3d+set-distance-fade-max-distance)

(defgproperty base-material-3d+stencil-mode 'base-material-3d :get
 'base-material-3d+get-stencil-mode :set 'base-material-3d+set-stencil-mode)

(defgproperty base-material-3d+stencil-flags 'base-material-3d :get
 'base-material-3d+get-stencil-flags :set 'base-material-3d+set-stencil-flags)

(defgproperty base-material-3d+stencil-compare 'base-material-3d :get
 'base-material-3d+get-stencil-compare :set
 'base-material-3d+set-stencil-compare)

(defgproperty base-material-3d+stencil-reference 'base-material-3d :get
 'base-material-3d+get-stencil-reference :set
 'base-material-3d+set-stencil-reference)

(defgproperty base-material-3d+stencil-color 'base-material-3d :get
 'base-material-3d+get-stencil-effect-color :set
 'base-material-3d+set-stencil-effect-color)

(defgproperty base-material-3d+stencil-outline-thickness 'base-material-3d :get
 'base-material-3d+get-stencil-effect-outline-thickness :set
 'base-material-3d+set-stencil-effect-outline-thickness)