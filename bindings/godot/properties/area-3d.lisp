(common-lisp:in-package :%godot)


(defgproperty area-3d+monitoring 'area-3d :get 'area-3d+is-monitoring :set
 'area-3d+set-monitoring)

(defgproperty area-3d+monitorable 'area-3d :get 'area-3d+is-monitorable :set
 'area-3d+set-monitorable)

(defgproperty area-3d+priority 'area-3d :get 'area-3d+get-priority :set
 'area-3d+set-priority)

(defgproperty area-3d+gravity-space-override 'area-3d :get
 'area-3d+get-gravity-space-override-mode :set
 'area-3d+set-gravity-space-override-mode)

(defgproperty area-3d+gravity-point 'area-3d :get 'area-3d+is-gravity-a-point
 :set 'area-3d+set-gravity-is-point)

(defgproperty area-3d+gravity-point-unit-distance 'area-3d :get
 'area-3d+get-gravity-point-unit-distance :set
 'area-3d+set-gravity-point-unit-distance)

(defgproperty area-3d+gravity-point-center 'area-3d :get
 'area-3d+get-gravity-point-center :set 'area-3d+set-gravity-point-center)

(defgproperty area-3d+gravity-direction 'area-3d :get
 'area-3d+get-gravity-direction :set 'area-3d+set-gravity-direction)

(defgproperty area-3d+gravity 'area-3d :get 'area-3d+get-gravity :set
 'area-3d+set-gravity)

(defgproperty area-3d+linear-damp-space-override 'area-3d :get
 'area-3d+get-linear-damp-space-override-mode :set
 'area-3d+set-linear-damp-space-override-mode)

(defgproperty area-3d+linear-damp 'area-3d :get 'area-3d+get-linear-damp :set
 'area-3d+set-linear-damp)

(defgproperty area-3d+angular-damp-space-override 'area-3d :get
 'area-3d+get-angular-damp-space-override-mode :set
 'area-3d+set-angular-damp-space-override-mode)

(defgproperty area-3d+angular-damp 'area-3d :get 'area-3d+get-angular-damp :set
 'area-3d+set-angular-damp)

(defgproperty area-3d+wind-force-magnitude 'area-3d :get
 'area-3d+get-wind-force-magnitude :set 'area-3d+set-wind-force-magnitude)

(defgproperty area-3d+wind-attenuation-factor 'area-3d :get
 'area-3d+get-wind-attenuation-factor :set 'area-3d+set-wind-attenuation-factor)

(defgproperty area-3d+wind-source-path 'area-3d :get
 'area-3d+get-wind-source-path :set 'area-3d+set-wind-source-path)

(defgproperty area-3d+audio-bus-override 'area-3d :get
 'area-3d+is-overriding-audio-bus :set 'area-3d+set-audio-bus-override)

(defgproperty area-3d+audio-bus-name 'area-3d :get 'area-3d+get-audio-bus-name
 :set 'area-3d+set-audio-bus-name)

(defgproperty area-3d+reverb-bus-enabled 'area-3d :get
 'area-3d+is-using-reverb-bus :set 'area-3d+set-use-reverb-bus)

(defgproperty area-3d+reverb-bus-name 'area-3d :get
 'area-3d+get-reverb-bus-name :set 'area-3d+set-reverb-bus-name)

(defgproperty area-3d+reverb-bus-amount 'area-3d :get
 'area-3d+get-reverb-amount :set 'area-3d+set-reverb-amount)

(defgproperty area-3d+reverb-bus-uniformity 'area-3d :get
 'area-3d+get-reverb-uniformity :set 'area-3d+set-reverb-uniformity)