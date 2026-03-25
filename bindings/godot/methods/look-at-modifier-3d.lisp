(common-lisp:in-package :%godot)


(defgmethod
 (look-at-modifier-3d+set-target-node :class 'look-at-modifier-3d :bind
  "set_target_node" :hash 1348162250)
 :void (target-node node-path))

(defgmethod
 (look-at-modifier-3d+get-target-node :class 'look-at-modifier-3d :bind
  "get_target_node" :hash 4075236667)
 node-path)

(defgmethod
 (look-at-modifier-3d+set-bone-name :class 'look-at-modifier-3d :bind
  "set_bone_name" :hash 83702148)
 :void (bone-name string))

(defgmethod
 (look-at-modifier-3d+get-bone-name :class 'look-at-modifier-3d :bind
  "get_bone_name" :hash 201670096)
 string)

(defgmethod
 (look-at-modifier-3d+set-bone :class 'look-at-modifier-3d :bind "set_bone"
  :hash 1286410249)
 :void (bone int))

(defgmethod
 (look-at-modifier-3d+get-bone :class 'look-at-modifier-3d :bind "get_bone"
  :hash 3905245786)
 int)

(defgmethod
 (look-at-modifier-3d+set-forward-axis :class 'look-at-modifier-3d :bind
  "set_forward_axis" :hash 3199955933)
 :void (forward-axis skeleton-modifier-3d+bone-axis))

(defgmethod
 (look-at-modifier-3d+get-forward-axis :class 'look-at-modifier-3d :bind
  "get_forward_axis" :hash 4076020284)
 skeleton-modifier-3d+bone-axis)

(defgmethod
 (look-at-modifier-3d+set-primary-rotation-axis :class 'look-at-modifier-3d
  :bind "set_primary_rotation_axis" :hash 1144690656)
 :void (axis vector-3+axis))

(defgmethod
 (look-at-modifier-3d+get-primary-rotation-axis :class 'look-at-modifier-3d
  :bind "get_primary_rotation_axis" :hash 3050976882)
 vector-3+axis)

(defgmethod
 (look-at-modifier-3d+set-use-secondary-rotation :class 'look-at-modifier-3d
  :bind "set_use_secondary_rotation" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (look-at-modifier-3d+is-using-secondary-rotation :class 'look-at-modifier-3d
  :bind "is_using_secondary_rotation" :hash 36873697)
 bool)

(defgmethod
 (look-at-modifier-3d+set-relative :class 'look-at-modifier-3d :bind
  "set_relative" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (look-at-modifier-3d+is-relative :class 'look-at-modifier-3d :bind
  "is_relative" :hash 36873697)
 bool)

(defgmethod
 (look-at-modifier-3d+set-origin-safe-margin :class 'look-at-modifier-3d :bind
  "set_origin_safe_margin" :hash 373806689)
 :void (margin float))

(defgmethod
 (look-at-modifier-3d+get-origin-safe-margin :class 'look-at-modifier-3d :bind
  "get_origin_safe_margin" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-origin-from :class 'look-at-modifier-3d :bind
  "set_origin_from" :hash 4254695669)
 :void (origin-from look-at-modifier-3d+origin-from))

(defgmethod
 (look-at-modifier-3d+get-origin-from :class 'look-at-modifier-3d :bind
  "get_origin_from" :hash 4057166297)
 look-at-modifier-3d+origin-from)

(defgmethod
 (look-at-modifier-3d+set-origin-bone-name :class 'look-at-modifier-3d :bind
  "set_origin_bone_name" :hash 83702148)
 :void (bone-name string))

(defgmethod
 (look-at-modifier-3d+get-origin-bone-name :class 'look-at-modifier-3d :bind
  "get_origin_bone_name" :hash 201670096)
 string)

(defgmethod
 (look-at-modifier-3d+set-origin-bone :class 'look-at-modifier-3d :bind
  "set_origin_bone" :hash 1286410249)
 :void (bone int))

(defgmethod
 (look-at-modifier-3d+get-origin-bone :class 'look-at-modifier-3d :bind
  "get_origin_bone" :hash 3905245786)
 int)

(defgmethod
 (look-at-modifier-3d+set-origin-external-node :class 'look-at-modifier-3d
  :bind "set_origin_external_node" :hash 1348162250)
 :void (external-node node-path))

(defgmethod
 (look-at-modifier-3d+get-origin-external-node :class 'look-at-modifier-3d
  :bind "get_origin_external_node" :hash 4075236667)
 node-path)

(defgmethod
 (look-at-modifier-3d+set-origin-offset :class 'look-at-modifier-3d :bind
  "set_origin_offset" :hash 3460891852)
 :void (offset vector-3))

(defgmethod
 (look-at-modifier-3d+get-origin-offset :class 'look-at-modifier-3d :bind
  "get_origin_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (look-at-modifier-3d+set-duration :class 'look-at-modifier-3d :bind
  "set_duration" :hash 373806689)
 :void (duration float))

(defgmethod
 (look-at-modifier-3d+get-duration :class 'look-at-modifier-3d :bind
  "get_duration" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-transition-type :class 'look-at-modifier-3d :bind
  "set_transition_type" :hash 1058637742)
 :void (transition-type tween+transition-type))

(defgmethod
 (look-at-modifier-3d+get-transition-type :class 'look-at-modifier-3d :bind
  "get_transition_type" :hash 3842314528)
 tween+transition-type)

(defgmethod
 (look-at-modifier-3d+set-ease-type :class 'look-at-modifier-3d :bind
  "set_ease_type" :hash 1208105857)
 :void (ease-type tween+ease-type))

(defgmethod
 (look-at-modifier-3d+get-ease-type :class 'look-at-modifier-3d :bind
  "get_ease_type" :hash 631880200)
 tween+ease-type)

(defgmethod
 (look-at-modifier-3d+set-use-angle-limitation :class 'look-at-modifier-3d
  :bind "set_use_angle_limitation" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (look-at-modifier-3d+is-using-angle-limitation :class 'look-at-modifier-3d
  :bind "is_using_angle_limitation" :hash 36873697)
 bool)

(defgmethod
 (look-at-modifier-3d+set-symmetry-limitation :class 'look-at-modifier-3d :bind
  "set_symmetry_limitation" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (look-at-modifier-3d+is-limitation-symmetry :class 'look-at-modifier-3d :bind
  "is_limitation_symmetry" :hash 36873697)
 bool)

(defgmethod
 (look-at-modifier-3d+set-primary-limit-angle :class 'look-at-modifier-3d :bind
  "set_primary_limit_angle" :hash 373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-primary-limit-angle :class 'look-at-modifier-3d :bind
  "get_primary_limit_angle" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-primary-damp-threshold :class 'look-at-modifier-3d
  :bind "set_primary_damp_threshold" :hash 373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-primary-damp-threshold :class 'look-at-modifier-3d
  :bind "get_primary_damp_threshold" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-primary-positive-limit-angle :class
  'look-at-modifier-3d :bind "set_primary_positive_limit_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-primary-positive-limit-angle :class
  'look-at-modifier-3d :bind "get_primary_positive_limit_angle" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-primary-positive-damp-threshold :class
  'look-at-modifier-3d :bind "set_primary_positive_damp_threshold" :hash
  373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-primary-positive-damp-threshold :class
  'look-at-modifier-3d :bind "get_primary_positive_damp_threshold" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-primary-negative-limit-angle :class
  'look-at-modifier-3d :bind "set_primary_negative_limit_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-primary-negative-limit-angle :class
  'look-at-modifier-3d :bind "get_primary_negative_limit_angle" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-primary-negative-damp-threshold :class
  'look-at-modifier-3d :bind "set_primary_negative_damp_threshold" :hash
  373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-primary-negative-damp-threshold :class
  'look-at-modifier-3d :bind "get_primary_negative_damp_threshold" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-limit-angle :class 'look-at-modifier-3d
  :bind "set_secondary_limit_angle" :hash 373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-secondary-limit-angle :class 'look-at-modifier-3d
  :bind "get_secondary_limit_angle" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-damp-threshold :class 'look-at-modifier-3d
  :bind "set_secondary_damp_threshold" :hash 373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-secondary-damp-threshold :class 'look-at-modifier-3d
  :bind "get_secondary_damp_threshold" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-positive-limit-angle :class
  'look-at-modifier-3d :bind "set_secondary_positive_limit_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-secondary-positive-limit-angle :class
  'look-at-modifier-3d :bind "get_secondary_positive_limit_angle" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-positive-damp-threshold :class
  'look-at-modifier-3d :bind "set_secondary_positive_damp_threshold" :hash
  373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-secondary-positive-damp-threshold :class
  'look-at-modifier-3d :bind "get_secondary_positive_damp_threshold" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-negative-limit-angle :class
  'look-at-modifier-3d :bind "set_secondary_negative_limit_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (look-at-modifier-3d+get-secondary-negative-limit-angle :class
  'look-at-modifier-3d :bind "get_secondary_negative_limit_angle" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+set-secondary-negative-damp-threshold :class
  'look-at-modifier-3d :bind "set_secondary_negative_damp_threshold" :hash
  373806689)
 :void (power float))

(defgmethod
 (look-at-modifier-3d+get-secondary-negative-damp-threshold :class
  'look-at-modifier-3d :bind "get_secondary_negative_damp_threshold" :hash
  1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+get-interpolation-remaining :class 'look-at-modifier-3d
  :bind "get_interpolation_remaining" :hash 1740695150)
 float)

(defgmethod
 (look-at-modifier-3d+is-interpolating :class 'look-at-modifier-3d :bind
  "is_interpolating" :hash 36873697)
 bool)

(defgmethod
 (look-at-modifier-3d+is-target-within-limitation :class 'look-at-modifier-3d
  :bind "is_target_within_limitation" :hash 36873697)
 bool)