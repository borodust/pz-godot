(common-lisp:in-package :%godot)


(defgproperty vehicle-wheel-3d+engine-force 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-engine-force :set 'vehicle-wheel-3d+set-engine-force)

(defgproperty vehicle-wheel-3d+brake 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-brake :set 'vehicle-wheel-3d+set-brake)

(defgproperty vehicle-wheel-3d+steering 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-steering :set 'vehicle-wheel-3d+set-steering)

(defgproperty vehicle-wheel-3d+use-as-traction 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+is-used-as-traction :set
 'vehicle-wheel-3d+set-use-as-traction)

(defgproperty vehicle-wheel-3d+use-as-steering 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+is-used-as-steering :set
 'vehicle-wheel-3d+set-use-as-steering)

(defgproperty vehicle-wheel-3d+wheel-roll-influence 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-roll-influence :set 'vehicle-wheel-3d+set-roll-influence)

(defgproperty vehicle-wheel-3d+wheel-radius 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-radius :set 'vehicle-wheel-3d+set-radius)

(defgproperty vehicle-wheel-3d+wheel-rest-length 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-suspension-rest-length :set
 'vehicle-wheel-3d+set-suspension-rest-length)

(defgproperty vehicle-wheel-3d+wheel-friction-slip 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-friction-slip :set 'vehicle-wheel-3d+set-friction-slip)

(defgproperty vehicle-wheel-3d+suspension-travel 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-suspension-travel :set
 'vehicle-wheel-3d+set-suspension-travel)

(defgproperty vehicle-wheel-3d+suspension-stiffness 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-suspension-stiffness :set
 'vehicle-wheel-3d+set-suspension-stiffness)

(defgproperty vehicle-wheel-3d+suspension-max-force 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-suspension-max-force :set
 'vehicle-wheel-3d+set-suspension-max-force)

(defgproperty vehicle-wheel-3d+damping-compression 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-damping-compression :set
 'vehicle-wheel-3d+set-damping-compression)

(defgproperty vehicle-wheel-3d+damping-relaxation 'vehicle-wheel-3d :get
 'vehicle-wheel-3d+get-damping-relaxation :set
 'vehicle-wheel-3d+set-damping-relaxation)