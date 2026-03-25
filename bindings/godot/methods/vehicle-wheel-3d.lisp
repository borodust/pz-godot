(common-lisp:in-package :%godot)


(defgmethod
 (vehicle-wheel-3d+set-radius :class 'vehicle-wheel-3d :bind "set_radius" :hash
  373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-radius :class 'vehicle-wheel-3d :bind "get_radius" :hash
  1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-suspension-rest-length :class 'vehicle-wheel-3d :bind
  "set_suspension_rest_length" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-suspension-rest-length :class 'vehicle-wheel-3d :bind
  "get_suspension_rest_length" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-suspension-travel :class 'vehicle-wheel-3d :bind
  "set_suspension_travel" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-suspension-travel :class 'vehicle-wheel-3d :bind
  "get_suspension_travel" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-suspension-stiffness :class 'vehicle-wheel-3d :bind
  "set_suspension_stiffness" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-suspension-stiffness :class 'vehicle-wheel-3d :bind
  "get_suspension_stiffness" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-suspension-max-force :class 'vehicle-wheel-3d :bind
  "set_suspension_max_force" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-suspension-max-force :class 'vehicle-wheel-3d :bind
  "get_suspension_max_force" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-damping-compression :class 'vehicle-wheel-3d :bind
  "set_damping_compression" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-damping-compression :class 'vehicle-wheel-3d :bind
  "get_damping_compression" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-damping-relaxation :class 'vehicle-wheel-3d :bind
  "set_damping_relaxation" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-damping-relaxation :class 'vehicle-wheel-3d :bind
  "get_damping_relaxation" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-use-as-traction :class 'vehicle-wheel-3d :bind
  "set_use_as_traction" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (vehicle-wheel-3d+is-used-as-traction :class 'vehicle-wheel-3d :bind
  "is_used_as_traction" :hash 36873697)
 bool)

(defgmethod
 (vehicle-wheel-3d+set-use-as-steering :class 'vehicle-wheel-3d :bind
  "set_use_as_steering" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (vehicle-wheel-3d+is-used-as-steering :class 'vehicle-wheel-3d :bind
  "is_used_as_steering" :hash 36873697)
 bool)

(defgmethod
 (vehicle-wheel-3d+set-friction-slip :class 'vehicle-wheel-3d :bind
  "set_friction_slip" :hash 373806689)
 :void (length float))

(defgmethod
 (vehicle-wheel-3d+get-friction-slip :class 'vehicle-wheel-3d :bind
  "get_friction_slip" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+is-in-contact :class 'vehicle-wheel-3d :bind "is_in_contact"
  :hash 36873697)
 bool)

(defgmethod
 (vehicle-wheel-3d+get-contact-body :class 'vehicle-wheel-3d :bind
  "get_contact_body" :hash 151077316)
 node-3d)

(defgmethod
 (vehicle-wheel-3d+get-contact-point :class 'vehicle-wheel-3d :bind
  "get_contact_point" :hash 3360562783)
 vector-3)

(defgmethod
 (vehicle-wheel-3d+get-contact-normal :class 'vehicle-wheel-3d :bind
  "get_contact_normal" :hash 3360562783)
 vector-3)

(defgmethod
 (vehicle-wheel-3d+set-roll-influence :class 'vehicle-wheel-3d :bind
  "set_roll_influence" :hash 373806689)
 :void (roll-influence float))

(defgmethod
 (vehicle-wheel-3d+get-roll-influence :class 'vehicle-wheel-3d :bind
  "get_roll_influence" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+get-skidinfo :class 'vehicle-wheel-3d :bind "get_skidinfo"
  :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+get-rpm :class 'vehicle-wheel-3d :bind "get_rpm" :hash
  1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-engine-force :class 'vehicle-wheel-3d :bind
  "set_engine_force" :hash 373806689)
 :void (engine-force float))

(defgmethod
 (vehicle-wheel-3d+get-engine-force :class 'vehicle-wheel-3d :bind
  "get_engine_force" :hash 1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-brake :class 'vehicle-wheel-3d :bind "set_brake" :hash
  373806689)
 :void (brake float))

(defgmethod
 (vehicle-wheel-3d+get-brake :class 'vehicle-wheel-3d :bind "get_brake" :hash
  1740695150)
 float)

(defgmethod
 (vehicle-wheel-3d+set-steering :class 'vehicle-wheel-3d :bind "set_steering"
  :hash 373806689)
 :void (steering float))

(defgmethod
 (vehicle-wheel-3d+get-steering :class 'vehicle-wheel-3d :bind "get_steering"
  :hash 1740695150)
 float)