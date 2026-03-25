(common-lisp:in-package :%godot)


(defgproperty path-follow-3d+progress 'path-follow-3d :get
 'path-follow-3d+get-progress :set 'path-follow-3d+set-progress)

(defgproperty path-follow-3d+progress-ratio 'path-follow-3d :get
 'path-follow-3d+get-progress-ratio :set 'path-follow-3d+set-progress-ratio)

(defgproperty path-follow-3d+h-offset 'path-follow-3d :get
 'path-follow-3d+get-h-offset :set 'path-follow-3d+set-h-offset)

(defgproperty path-follow-3d+v-offset 'path-follow-3d :get
 'path-follow-3d+get-v-offset :set 'path-follow-3d+set-v-offset)

(defgproperty path-follow-3d+rotation-mode 'path-follow-3d :get
 'path-follow-3d+get-rotation-mode :set 'path-follow-3d+set-rotation-mode)

(defgproperty path-follow-3d+use-model-front 'path-follow-3d :get
 'path-follow-3d+is-using-model-front :set 'path-follow-3d+set-use-model-front)

(defgproperty path-follow-3d+cubic-interp 'path-follow-3d :get
 'path-follow-3d+get-cubic-interpolation :set
 'path-follow-3d+set-cubic-interpolation)

(defgproperty path-follow-3d+loop 'path-follow-3d :get 'path-follow-3d+has-loop
 :set 'path-follow-3d+set-loop)

(defgproperty path-follow-3d+tilt-enabled 'path-follow-3d :get
 'path-follow-3d+is-tilt-enabled :set 'path-follow-3d+set-tilt-enabled)