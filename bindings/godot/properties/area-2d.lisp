(common-lisp:in-package :%godot)


(defgproperty area-2d+monitoring 'area-2d :get 'area-2d+is-monitoring :set
 'area-2d+set-monitoring)

(defgproperty area-2d+monitorable 'area-2d :get 'area-2d+is-monitorable :set
 'area-2d+set-monitorable)

(defgproperty area-2d+priority 'area-2d :get 'area-2d+get-priority :set
 'area-2d+set-priority)

(defgproperty area-2d+gravity-space-override 'area-2d :get
 'area-2d+get-gravity-space-override-mode :set
 'area-2d+set-gravity-space-override-mode)

(defgproperty area-2d+gravity-point 'area-2d :get 'area-2d+is-gravity-a-point
 :set 'area-2d+set-gravity-is-point)

(defgproperty area-2d+gravity-point-unit-distance 'area-2d :get
 'area-2d+get-gravity-point-unit-distance :set
 'area-2d+set-gravity-point-unit-distance)

(defgproperty area-2d+gravity-point-center 'area-2d :get
 'area-2d+get-gravity-point-center :set 'area-2d+set-gravity-point-center)

(defgproperty area-2d+gravity-direction 'area-2d :get
 'area-2d+get-gravity-direction :set 'area-2d+set-gravity-direction)

(defgproperty area-2d+gravity 'area-2d :get 'area-2d+get-gravity :set
 'area-2d+set-gravity)

(defgproperty area-2d+linear-damp-space-override 'area-2d :get
 'area-2d+get-linear-damp-space-override-mode :set
 'area-2d+set-linear-damp-space-override-mode)

(defgproperty area-2d+linear-damp 'area-2d :get 'area-2d+get-linear-damp :set
 'area-2d+set-linear-damp)

(defgproperty area-2d+angular-damp-space-override 'area-2d :get
 'area-2d+get-angular-damp-space-override-mode :set
 'area-2d+set-angular-damp-space-override-mode)

(defgproperty area-2d+angular-damp 'area-2d :get 'area-2d+get-angular-damp :set
 'area-2d+set-angular-damp)

(defgproperty area-2d+audio-bus-override 'area-2d :get
 'area-2d+is-overriding-audio-bus :set 'area-2d+set-audio-bus-override)

(defgproperty area-2d+audio-bus-name 'area-2d :get 'area-2d+get-audio-bus-name
 :set 'area-2d+set-audio-bus-name)