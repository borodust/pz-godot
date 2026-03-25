(common-lisp:in-package :%godot)


(defgmethod
 (physical-bone-3d+-integrate-forces :class 'physical-bone-3d :bind
  "_integrate_forces" :hash 420958145 :virtual common-lisp:t)
 :void (state physics-direct-body-state-3d))

(defgmethod
 (physical-bone-3d+apply-central-impulse :class 'physical-bone-3d :bind
  "apply_central_impulse" :hash 3460891852)
 :void (impulse vector-3))

(defgmethod
 (physical-bone-3d+apply-impulse :class 'physical-bone-3d :bind "apply_impulse"
  :hash 2754756483)
 :void (impulse vector-3) (position vector-3))

(defgmethod
 (physical-bone-3d+set-joint-type :class 'physical-bone-3d :bind
  "set_joint_type" :hash 2289552604)
 :void (joint-type physical-bone-3d+joint-type))

(defgmethod
 (physical-bone-3d+get-joint-type :class 'physical-bone-3d :bind
  "get_joint_type" :hash 931347320)
 physical-bone-3d+joint-type)

(defgmethod
 (physical-bone-3d+set-joint-offset :class 'physical-bone-3d :bind
  "set_joint_offset" :hash 2952846383)
 :void (offset transform-3d))

(defgmethod
 (physical-bone-3d+get-joint-offset :class 'physical-bone-3d :bind
  "get_joint_offset" :hash 3229777777)
 transform-3d)

(defgmethod
 (physical-bone-3d+set-joint-rotation :class 'physical-bone-3d :bind
  "set_joint_rotation" :hash 3460891852)
 :void (euler vector-3))

(defgmethod
 (physical-bone-3d+get-joint-rotation :class 'physical-bone-3d :bind
  "get_joint_rotation" :hash 3360562783)
 vector-3)

(defgmethod
 (physical-bone-3d+set-body-offset :class 'physical-bone-3d :bind
  "set_body_offset" :hash 2952846383)
 :void (offset transform-3d))

(defgmethod
 (physical-bone-3d+get-body-offset :class 'physical-bone-3d :bind
  "get_body_offset" :hash 3229777777)
 transform-3d)

(defgmethod
 (physical-bone-3d+get-simulate-physics :class 'physical-bone-3d :bind
  "get_simulate_physics" :hash 2240911060)
 bool)

(defgmethod
 (physical-bone-3d+is-simulating-physics :class 'physical-bone-3d :bind
  "is_simulating_physics" :hash 2240911060)
 bool)

(defgmethod
 (physical-bone-3d+get-bone-id :class 'physical-bone-3d :bind "get_bone_id"
  :hash 3905245786)
 int)

(defgmethod
 (physical-bone-3d+set-mass :class 'physical-bone-3d :bind "set_mass" :hash
  373806689)
 :void (mass float))

(defgmethod
 (physical-bone-3d+get-mass :class 'physical-bone-3d :bind "get_mass" :hash
  1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-friction :class 'physical-bone-3d :bind "set_friction"
  :hash 373806689)
 :void (friction float))

(defgmethod
 (physical-bone-3d+get-friction :class 'physical-bone-3d :bind "get_friction"
  :hash 1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-bounce :class 'physical-bone-3d :bind "set_bounce" :hash
  373806689)
 :void (bounce float))

(defgmethod
 (physical-bone-3d+get-bounce :class 'physical-bone-3d :bind "get_bounce" :hash
  1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-gravity-scale :class 'physical-bone-3d :bind
  "set_gravity_scale" :hash 373806689)
 :void (gravity-scale float))

(defgmethod
 (physical-bone-3d+get-gravity-scale :class 'physical-bone-3d :bind
  "get_gravity_scale" :hash 1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-linear-damp-mode :class 'physical-bone-3d :bind
  "set_linear_damp_mode" :hash 1244972221)
 :void (linear-damp-mode physical-bone-3d+damp-mode))

(defgmethod
 (physical-bone-3d+get-linear-damp-mode :class 'physical-bone-3d :bind
  "get_linear_damp_mode" :hash 205884699)
 physical-bone-3d+damp-mode)

(defgmethod
 (physical-bone-3d+set-angular-damp-mode :class 'physical-bone-3d :bind
  "set_angular_damp_mode" :hash 1244972221)
 :void (angular-damp-mode physical-bone-3d+damp-mode))

(defgmethod
 (physical-bone-3d+get-angular-damp-mode :class 'physical-bone-3d :bind
  "get_angular_damp_mode" :hash 205884699)
 physical-bone-3d+damp-mode)

(defgmethod
 (physical-bone-3d+set-linear-damp :class 'physical-bone-3d :bind
  "set_linear_damp" :hash 373806689)
 :void (linear-damp float))

(defgmethod
 (physical-bone-3d+get-linear-damp :class 'physical-bone-3d :bind
  "get_linear_damp" :hash 1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-angular-damp :class 'physical-bone-3d :bind
  "set_angular_damp" :hash 373806689)
 :void (angular-damp float))

(defgmethod
 (physical-bone-3d+get-angular-damp :class 'physical-bone-3d :bind
  "get_angular_damp" :hash 1740695150)
 float)

(defgmethod
 (physical-bone-3d+set-linear-velocity :class 'physical-bone-3d :bind
  "set_linear_velocity" :hash 3460891852)
 :void (linear-velocity vector-3))

(defgmethod
 (physical-bone-3d+get-linear-velocity :class 'physical-bone-3d :bind
  "get_linear_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (physical-bone-3d+set-angular-velocity :class 'physical-bone-3d :bind
  "set_angular_velocity" :hash 3460891852)
 :void (angular-velocity vector-3))

(defgmethod
 (physical-bone-3d+get-angular-velocity :class 'physical-bone-3d :bind
  "get_angular_velocity" :hash 3360562783)
 vector-3)

(defgmethod
 (physical-bone-3d+set-use-custom-integrator :class 'physical-bone-3d :bind
  "set_use_custom_integrator" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (physical-bone-3d+is-using-custom-integrator :class 'physical-bone-3d :bind
  "is_using_custom_integrator" :hash 2240911060)
 bool)

(defgmethod
 (physical-bone-3d+set-can-sleep :class 'physical-bone-3d :bind "set_can_sleep"
  :hash 2586408642)
 :void (able-to-sleep bool))

(defgmethod
 (physical-bone-3d+is-able-to-sleep :class 'physical-bone-3d :bind
  "is_able_to_sleep" :hash 36873697)
 bool)