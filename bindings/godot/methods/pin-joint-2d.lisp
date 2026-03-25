(common-lisp:in-package :%godot)


(defgmethod
 (pin-joint-2d+set-softness :class 'pin-joint-2d :bind "set_softness" :hash
  373806689)
 :void (softness float))

(defgmethod
 (pin-joint-2d+get-softness :class 'pin-joint-2d :bind "get_softness" :hash
  1740695150)
 float)

(defgmethod
 (pin-joint-2d+set-angular-limit-lower :class 'pin-joint-2d :bind
  "set_angular_limit_lower" :hash 373806689)
 :void (angular-limit-lower float))

(defgmethod
 (pin-joint-2d+get-angular-limit-lower :class 'pin-joint-2d :bind
  "get_angular_limit_lower" :hash 1740695150)
 float)

(defgmethod
 (pin-joint-2d+set-angular-limit-upper :class 'pin-joint-2d :bind
  "set_angular_limit_upper" :hash 373806689)
 :void (angular-limit-upper float))

(defgmethod
 (pin-joint-2d+get-angular-limit-upper :class 'pin-joint-2d :bind
  "get_angular_limit_upper" :hash 1740695150)
 float)

(defgmethod
 (pin-joint-2d+set-motor-target-velocity :class 'pin-joint-2d :bind
  "set_motor_target_velocity" :hash 373806689)
 :void (motor-target-velocity float))

(defgmethod
 (pin-joint-2d+get-motor-target-velocity :class 'pin-joint-2d :bind
  "get_motor_target_velocity" :hash 1740695150)
 float)

(defgmethod
 (pin-joint-2d+set-motor-enabled :class 'pin-joint-2d :bind "set_motor_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (pin-joint-2d+is-motor-enabled :class 'pin-joint-2d :bind "is_motor_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (pin-joint-2d+set-angular-limit-enabled :class 'pin-joint-2d :bind
  "set_angular_limit_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (pin-joint-2d+is-angular-limit-enabled :class 'pin-joint-2d :bind
  "is_angular_limit_enabled" :hash 36873697)
 bool)