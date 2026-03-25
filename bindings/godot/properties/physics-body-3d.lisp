(common-lisp:in-package :%godot)


(defgproperty physics-body-3d+axis-lock-linear-x 'physics-body-3d :index 1 :get
 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)

(defgproperty physics-body-3d+axis-lock-linear-y 'physics-body-3d :index 2 :get
 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)

(defgproperty physics-body-3d+axis-lock-linear-z 'physics-body-3d :index 4 :get
 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)

(defgproperty physics-body-3d+axis-lock-angular-x 'physics-body-3d :index 8
 :get 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)

(defgproperty physics-body-3d+axis-lock-angular-y 'physics-body-3d :index 16
 :get 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)

(defgproperty physics-body-3d+axis-lock-angular-z 'physics-body-3d :index 32
 :get 'physics-body-3d+get-axis-lock :set 'physics-body-3d+set-axis-lock)