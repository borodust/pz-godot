(common-lisp:in-package :%godot)


(defgproperty light-3d+light-intensity-lumens 'light-3d :index 20 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-intensity-lux 'light-3d :index 20 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-temperature 'light-3d :get
 'light-3d+get-temperature :set 'light-3d+set-temperature)

(defgproperty light-3d+light-color 'light-3d :get 'light-3d+get-color :set
 'light-3d+set-color)

(defgproperty light-3d+light-energy 'light-3d :index 0 :get 'light-3d+get-param
 :set 'light-3d+set-param)

(defgproperty light-3d+light-indirect-energy 'light-3d :index 1 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-volumetric-fog-energy 'light-3d :index 2 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-projector 'light-3d :get 'light-3d+get-projector
 :set 'light-3d+set-projector)

(defgproperty light-3d+light-size 'light-3d :index 5 :get 'light-3d+get-param
 :set 'light-3d+set-param)

(defgproperty light-3d+light-angular-distance 'light-3d :index 5 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-negative 'light-3d :get 'light-3d+is-negative :set
 'light-3d+set-negative)

(defgproperty light-3d+light-specular 'light-3d :index 3 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+light-bake-mode 'light-3d :get 'light-3d+get-bake-mode
 :set 'light-3d+set-bake-mode)

(defgproperty light-3d+light-cull-mask 'light-3d :get 'light-3d+get-cull-mask
 :set 'light-3d+set-cull-mask)

(defgproperty light-3d+shadow-enabled 'light-3d :get 'light-3d+has-shadow :set
 'light-3d+set-shadow)

(defgproperty light-3d+shadow-bias 'light-3d :index 15 :get 'light-3d+get-param
 :set 'light-3d+set-param)

(defgproperty light-3d+shadow-normal-bias 'light-3d :index 14 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+shadow-reverse-cull-face 'light-3d :get
 'light-3d+get-shadow-reverse-cull-face :set
 'light-3d+set-shadow-reverse-cull-face)

(defgproperty light-3d+shadow-transmittance-bias 'light-3d :index 19 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+shadow-opacity 'light-3d :index 17 :get
 'light-3d+get-param :set 'light-3d+set-param)

(defgproperty light-3d+shadow-blur 'light-3d :index 18 :get 'light-3d+get-param
 :set 'light-3d+set-param)

(defgproperty light-3d+shadow-caster-mask 'light-3d :get
 'light-3d+get-shadow-caster-mask :set 'light-3d+set-shadow-caster-mask)

(defgproperty light-3d+distance-fade-enabled 'light-3d :get
 'light-3d+is-distance-fade-enabled :set 'light-3d+set-enable-distance-fade)

(defgproperty light-3d+distance-fade-begin 'light-3d :get
 'light-3d+get-distance-fade-begin :set 'light-3d+set-distance-fade-begin)

(defgproperty light-3d+distance-fade-shadow 'light-3d :get
 'light-3d+get-distance-fade-shadow :set 'light-3d+set-distance-fade-shadow)

(defgproperty light-3d+distance-fade-length 'light-3d :get
 'light-3d+get-distance-fade-length :set 'light-3d+set-distance-fade-length)

(defgproperty light-3d+editor-only 'light-3d :get 'light-3d+is-editor-only :set
 'light-3d+set-editor-only)