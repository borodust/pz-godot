(common-lisp:in-package :%godot)


(defgproperty environment+background-mode 'environment :get
 'environment+get-background :set 'environment+set-background)

(defgproperty environment+background-color 'environment :get
 'environment+get-bg-color :set 'environment+set-bg-color)

(defgproperty environment+background-energy-multiplier 'environment :get
 'environment+get-bg-energy-multiplier :set
 'environment+set-bg-energy-multiplier)

(defgproperty environment+background-intensity 'environment :get
 'environment+get-bg-intensity :set 'environment+set-bg-intensity)

(defgproperty environment+background-canvas-max-layer 'environment :get
 'environment+get-canvas-max-layer :set 'environment+set-canvas-max-layer)

(defgproperty environment+background-camera-feed-id 'environment :get
 'environment+get-camera-feed-id :set 'environment+set-camera-feed-id)

(defgproperty environment+sky 'environment :get 'environment+get-sky :set
 'environment+set-sky)

(defgproperty environment+sky-custom-fov 'environment :get
 'environment+get-sky-custom-fov :set 'environment+set-sky-custom-fov)

(defgproperty environment+sky-rotation 'environment :get
 'environment+get-sky-rotation :set 'environment+set-sky-rotation)

(defgproperty environment+ambient-light-source 'environment :get
 'environment+get-ambient-source :set 'environment+set-ambient-source)

(defgproperty environment+ambient-light-color 'environment :get
 'environment+get-ambient-light-color :set 'environment+set-ambient-light-color)

(defgproperty environment+ambient-light-sky-contribution 'environment :get
 'environment+get-ambient-light-sky-contribution :set
 'environment+set-ambient-light-sky-contribution)

(defgproperty environment+ambient-light-energy 'environment :get
 'environment+get-ambient-light-energy :set
 'environment+set-ambient-light-energy)

(defgproperty environment+reflected-light-source 'environment :get
 'environment+get-reflection-source :set 'environment+set-reflection-source)

(defgproperty environment+tonemap-mode 'environment :get
 'environment+get-tonemapper :set 'environment+set-tonemapper)

(defgproperty environment+tonemap-exposure 'environment :get
 'environment+get-tonemap-exposure :set 'environment+set-tonemap-exposure)

(defgproperty environment+tonemap-white 'environment :get
 'environment+get-tonemap-white :set 'environment+set-tonemap-white)

(defgproperty environment+tonemap-agx-white 'environment :get
 'environment+get-tonemap-agx-white :set 'environment+set-tonemap-agx-white)

(defgproperty environment+tonemap-agx-contrast 'environment :get
 'environment+get-tonemap-agx-contrast :set
 'environment+set-tonemap-agx-contrast)

(defgproperty environment+ssr-enabled 'environment :get
 'environment+is-ssr-enabled :set 'environment+set-ssr-enabled)

(defgproperty environment+ssr-max-steps 'environment :get
 'environment+get-ssr-max-steps :set 'environment+set-ssr-max-steps)

(defgproperty environment+ssr-fade-in 'environment :get
 'environment+get-ssr-fade-in :set 'environment+set-ssr-fade-in)

(defgproperty environment+ssr-fade-out 'environment :get
 'environment+get-ssr-fade-out :set 'environment+set-ssr-fade-out)

(defgproperty environment+ssr-depth-tolerance 'environment :get
 'environment+get-ssr-depth-tolerance :set 'environment+set-ssr-depth-tolerance)

(defgproperty environment+ssao-enabled 'environment :get
 'environment+is-ssao-enabled :set 'environment+set-ssao-enabled)

(defgproperty environment+ssao-radius 'environment :get
 'environment+get-ssao-radius :set 'environment+set-ssao-radius)

(defgproperty environment+ssao-intensity 'environment :get
 'environment+get-ssao-intensity :set 'environment+set-ssao-intensity)

(defgproperty environment+ssao-power 'environment :get
 'environment+get-ssao-power :set 'environment+set-ssao-power)

(defgproperty environment+ssao-detail 'environment :get
 'environment+get-ssao-detail :set 'environment+set-ssao-detail)

(defgproperty environment+ssao-horizon 'environment :get
 'environment+get-ssao-horizon :set 'environment+set-ssao-horizon)

(defgproperty environment+ssao-sharpness 'environment :get
 'environment+get-ssao-sharpness :set 'environment+set-ssao-sharpness)

(defgproperty environment+ssao-light-affect 'environment :get
 'environment+get-ssao-direct-light-affect :set
 'environment+set-ssao-direct-light-affect)

(defgproperty environment+ssao-ao-channel-affect 'environment :get
 'environment+get-ssao-ao-channel-affect :set
 'environment+set-ssao-ao-channel-affect)

(defgproperty environment+ssil-enabled 'environment :get
 'environment+is-ssil-enabled :set 'environment+set-ssil-enabled)

(defgproperty environment+ssil-radius 'environment :get
 'environment+get-ssil-radius :set 'environment+set-ssil-radius)

(defgproperty environment+ssil-intensity 'environment :get
 'environment+get-ssil-intensity :set 'environment+set-ssil-intensity)

(defgproperty environment+ssil-sharpness 'environment :get
 'environment+get-ssil-sharpness :set 'environment+set-ssil-sharpness)

(defgproperty environment+ssil-normal-rejection 'environment :get
 'environment+get-ssil-normal-rejection :set
 'environment+set-ssil-normal-rejection)

(defgproperty environment+sdfgi-enabled 'environment :get
 'environment+is-sdfgi-enabled :set 'environment+set-sdfgi-enabled)

(defgproperty environment+sdfgi-use-occlusion 'environment :get
 'environment+is-sdfgi-using-occlusion :set
 'environment+set-sdfgi-use-occlusion)

(defgproperty environment+sdfgi-read-sky-light 'environment :get
 'environment+is-sdfgi-reading-sky-light :set
 'environment+set-sdfgi-read-sky-light)

(defgproperty environment+sdfgi-bounce-feedback 'environment :get
 'environment+get-sdfgi-bounce-feedback :set
 'environment+set-sdfgi-bounce-feedback)

(defgproperty environment+sdfgi-cascades 'environment :get
 'environment+get-sdfgi-cascades :set 'environment+set-sdfgi-cascades)

(defgproperty environment+sdfgi-min-cell-size 'environment :get
 'environment+get-sdfgi-min-cell-size :set 'environment+set-sdfgi-min-cell-size)

(defgproperty environment+sdfgi-cascade0-distance 'environment :get
 'environment+get-sdfgi-cascade0-distance :set
 'environment+set-sdfgi-cascade0-distance)

(defgproperty environment+sdfgi-max-distance 'environment :get
 'environment+get-sdfgi-max-distance :set 'environment+set-sdfgi-max-distance)

(defgproperty environment+sdfgi-y-scale 'environment :get
 'environment+get-sdfgi-y-scale :set 'environment+set-sdfgi-y-scale)

(defgproperty environment+sdfgi-energy 'environment :get
 'environment+get-sdfgi-energy :set 'environment+set-sdfgi-energy)

(defgproperty environment+sdfgi-normal-bias 'environment :get
 'environment+get-sdfgi-normal-bias :set 'environment+set-sdfgi-normal-bias)

(defgproperty environment+sdfgi-probe-bias 'environment :get
 'environment+get-sdfgi-probe-bias :set 'environment+set-sdfgi-probe-bias)

(defgproperty environment+glow-enabled 'environment :get
 'environment+is-glow-enabled :set 'environment+set-glow-enabled)

(defgproperty environment+glow-normalized 'environment :get
 'environment+is-glow-normalized :set 'environment+set-glow-normalized)

(defgproperty environment+glow-intensity 'environment :get
 'environment+get-glow-intensity :set 'environment+set-glow-intensity)

(defgproperty environment+glow-strength 'environment :get
 'environment+get-glow-strength :set 'environment+set-glow-strength)

(defgproperty environment+glow-mix 'environment :get 'environment+get-glow-mix
 :set 'environment+set-glow-mix)

(defgproperty environment+glow-bloom 'environment :get
 'environment+get-glow-bloom :set 'environment+set-glow-bloom)

(defgproperty environment+glow-blend-mode 'environment :get
 'environment+get-glow-blend-mode :set 'environment+set-glow-blend-mode)

(defgproperty environment+glow-hdr-threshold 'environment :get
 'environment+get-glow-hdr-bleed-threshold :set
 'environment+set-glow-hdr-bleed-threshold)

(defgproperty environment+glow-hdr-scale 'environment :get
 'environment+get-glow-hdr-bleed-scale :set
 'environment+set-glow-hdr-bleed-scale)

(defgproperty environment+glow-hdr-luminance-cap 'environment :get
 'environment+get-glow-hdr-luminance-cap :set
 'environment+set-glow-hdr-luminance-cap)

(defgproperty environment+glow-map-strength 'environment :get
 'environment+get-glow-map-strength :set 'environment+set-glow-map-strength)

(defgproperty environment+glow-map 'environment :get 'environment+get-glow-map
 :set 'environment+set-glow-map)

(defgproperty environment+fog-enabled 'environment :get
 'environment+is-fog-enabled :set 'environment+set-fog-enabled)

(defgproperty environment+fog-mode 'environment :get 'environment+get-fog-mode
 :set 'environment+set-fog-mode)

(defgproperty environment+fog-light-color 'environment :get
 'environment+get-fog-light-color :set 'environment+set-fog-light-color)

(defgproperty environment+fog-light-energy 'environment :get
 'environment+get-fog-light-energy :set 'environment+set-fog-light-energy)

(defgproperty environment+fog-sun-scatter 'environment :get
 'environment+get-fog-sun-scatter :set 'environment+set-fog-sun-scatter)

(defgproperty environment+fog-density 'environment :get
 'environment+get-fog-density :set 'environment+set-fog-density)

(defgproperty environment+fog-aerial-perspective 'environment :get
 'environment+get-fog-aerial-perspective :set
 'environment+set-fog-aerial-perspective)

(defgproperty environment+fog-sky-affect 'environment :get
 'environment+get-fog-sky-affect :set 'environment+set-fog-sky-affect)

(defgproperty environment+fog-height 'environment :get
 'environment+get-fog-height :set 'environment+set-fog-height)

(defgproperty environment+fog-height-density 'environment :get
 'environment+get-fog-height-density :set 'environment+set-fog-height-density)

(defgproperty environment+fog-depth-curve 'environment :get
 'environment+get-fog-depth-curve :set 'environment+set-fog-depth-curve)

(defgproperty environment+fog-depth-begin 'environment :get
 'environment+get-fog-depth-begin :set 'environment+set-fog-depth-begin)

(defgproperty environment+fog-depth-end 'environment :get
 'environment+get-fog-depth-end :set 'environment+set-fog-depth-end)

(defgproperty environment+volumetric-fog-enabled 'environment :get
 'environment+is-volumetric-fog-enabled :set
 'environment+set-volumetric-fog-enabled)

(defgproperty environment+volumetric-fog-density 'environment :get
 'environment+get-volumetric-fog-density :set
 'environment+set-volumetric-fog-density)

(defgproperty environment+volumetric-fog-albedo 'environment :get
 'environment+get-volumetric-fog-albedo :set
 'environment+set-volumetric-fog-albedo)

(defgproperty environment+volumetric-fog-emission 'environment :get
 'environment+get-volumetric-fog-emission :set
 'environment+set-volumetric-fog-emission)

(defgproperty environment+volumetric-fog-emission-energy 'environment :get
 'environment+get-volumetric-fog-emission-energy :set
 'environment+set-volumetric-fog-emission-energy)

(defgproperty environment+volumetric-fog-gi-inject 'environment :get
 'environment+get-volumetric-fog-gi-inject :set
 'environment+set-volumetric-fog-gi-inject)

(defgproperty environment+volumetric-fog-anisotropy 'environment :get
 'environment+get-volumetric-fog-anisotropy :set
 'environment+set-volumetric-fog-anisotropy)

(defgproperty environment+volumetric-fog-length 'environment :get
 'environment+get-volumetric-fog-length :set
 'environment+set-volumetric-fog-length)

(defgproperty environment+volumetric-fog-detail-spread 'environment :get
 'environment+get-volumetric-fog-detail-spread :set
 'environment+set-volumetric-fog-detail-spread)

(defgproperty environment+volumetric-fog-ambient-inject 'environment :get
 'environment+get-volumetric-fog-ambient-inject :set
 'environment+set-volumetric-fog-ambient-inject)

(defgproperty environment+volumetric-fog-sky-affect 'environment :get
 'environment+get-volumetric-fog-sky-affect :set
 'environment+set-volumetric-fog-sky-affect)

(defgproperty environment+volumetric-fog-temporal-reprojection-enabled
 'environment :get 'environment+is-volumetric-fog-temporal-reprojection-enabled
 :set 'environment+set-volumetric-fog-temporal-reprojection-enabled)

(defgproperty environment+volumetric-fog-temporal-reprojection-amount
 'environment :get 'environment+get-volumetric-fog-temporal-reprojection-amount
 :set 'environment+set-volumetric-fog-temporal-reprojection-amount)

(defgproperty environment+adjustment-enabled 'environment :get
 'environment+is-adjustment-enabled :set 'environment+set-adjustment-enabled)

(defgproperty environment+adjustment-brightness 'environment :get
 'environment+get-adjustment-brightness :set
 'environment+set-adjustment-brightness)

(defgproperty environment+adjustment-contrast 'environment :get
 'environment+get-adjustment-contrast :set 'environment+set-adjustment-contrast)

(defgproperty environment+adjustment-saturation 'environment :get
 'environment+get-adjustment-saturation :set
 'environment+set-adjustment-saturation)

(defgproperty environment+adjustment-color-correction 'environment :get
 'environment+get-adjustment-color-correction :set
 'environment+set-adjustment-color-correction)