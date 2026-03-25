(common-lisp:in-package :%godot)


(defgproperty lightmap-gi+quality 'lightmap-gi :get
 'lightmap-gi+get-bake-quality :set 'lightmap-gi+set-bake-quality)

(defgproperty lightmap-gi+supersampling 'lightmap-gi :get
 'lightmap-gi+is-supersampling-enabled :set
 'lightmap-gi+set-supersampling-enabled)

(defgproperty lightmap-gi+supersampling-factor 'lightmap-gi :get
 'lightmap-gi+get-supersampling-factor :set
 'lightmap-gi+set-supersampling-factor)

(defgproperty lightmap-gi+bounces 'lightmap-gi :get 'lightmap-gi+get-bounces
 :set 'lightmap-gi+set-bounces)

(defgproperty lightmap-gi+bounce-indirect-energy 'lightmap-gi :get
 'lightmap-gi+get-bounce-indirect-energy :set
 'lightmap-gi+set-bounce-indirect-energy)

(defgproperty lightmap-gi+directional 'lightmap-gi :get
 'lightmap-gi+is-directional :set 'lightmap-gi+set-directional)

(defgproperty lightmap-gi+shadowmask-mode 'lightmap-gi :get
 'lightmap-gi+get-shadowmask-mode :set 'lightmap-gi+set-shadowmask-mode)

(defgproperty lightmap-gi+use-texture-for-bounces 'lightmap-gi :get
 'lightmap-gi+is-using-texture-for-bounces :set
 'lightmap-gi+set-use-texture-for-bounces)

(defgproperty lightmap-gi+interior 'lightmap-gi :get 'lightmap-gi+is-interior
 :set 'lightmap-gi+set-interior)

(defgproperty lightmap-gi+use-denoiser 'lightmap-gi :get
 'lightmap-gi+is-using-denoiser :set 'lightmap-gi+set-use-denoiser)

(defgproperty lightmap-gi+denoiser-strength 'lightmap-gi :get
 'lightmap-gi+get-denoiser-strength :set 'lightmap-gi+set-denoiser-strength)

(defgproperty lightmap-gi+denoiser-range 'lightmap-gi :get
 'lightmap-gi+get-denoiser-range :set 'lightmap-gi+set-denoiser-range)

(defgproperty lightmap-gi+bias 'lightmap-gi :get 'lightmap-gi+get-bias :set
 'lightmap-gi+set-bias)

(defgproperty lightmap-gi+texel-scale 'lightmap-gi :get
 'lightmap-gi+get-texel-scale :set 'lightmap-gi+set-texel-scale)

(defgproperty lightmap-gi+max-texture-size 'lightmap-gi :get
 'lightmap-gi+get-max-texture-size :set 'lightmap-gi+set-max-texture-size)

(defgproperty lightmap-gi+environment-mode 'lightmap-gi :get
 'lightmap-gi+get-environment-mode :set 'lightmap-gi+set-environment-mode)

(defgproperty lightmap-gi+environment-custom-sky 'lightmap-gi :get
 'lightmap-gi+get-environment-custom-sky :set
 'lightmap-gi+set-environment-custom-sky)

(defgproperty lightmap-gi+environment-custom-color 'lightmap-gi :get
 'lightmap-gi+get-environment-custom-color :set
 'lightmap-gi+set-environment-custom-color)

(defgproperty lightmap-gi+environment-custom-energy 'lightmap-gi :get
 'lightmap-gi+get-environment-custom-energy :set
 'lightmap-gi+set-environment-custom-energy)

(defgproperty lightmap-gi+camera-attributes 'lightmap-gi :get
 'lightmap-gi+get-camera-attributes :set 'lightmap-gi+set-camera-attributes)

(defgproperty lightmap-gi+generate-probes-subdiv 'lightmap-gi :get
 'lightmap-gi+get-generate-probes :set 'lightmap-gi+set-generate-probes)

(defgproperty lightmap-gi+light-data 'lightmap-gi :get
 'lightmap-gi+get-light-data :set 'lightmap-gi+set-light-data)