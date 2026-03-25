(common-lisp:in-package :%godot)


(defgmethod
 (iterate-ik3d+set-max-iterations :class 'iterate-ik3d :bind
  "set_max_iterations" :hash 1286410249)
 :void (max-iterations int))

(defgmethod
 (iterate-ik3d+get-max-iterations :class 'iterate-ik3d :bind
  "get_max_iterations" :hash 3905245786)
 int)

(defgmethod
 (iterate-ik3d+set-min-distance :class 'iterate-ik3d :bind "set_min_distance"
  :hash 373806689)
 :void (min-distance float))

(defgmethod
 (iterate-ik3d+get-min-distance :class 'iterate-ik3d :bind "get_min_distance"
  :hash 1740695150)
 float)

(defgmethod
 (iterate-ik3d+set-angular-delta-limit :class 'iterate-ik3d :bind
  "set_angular_delta_limit" :hash 373806689)
 :void (angular-delta-limit float))

(defgmethod
 (iterate-ik3d+get-angular-delta-limit :class 'iterate-ik3d :bind
  "get_angular_delta_limit" :hash 1740695150)
 float)

(defgmethod
 (iterate-ik3d+set-deterministic :class 'iterate-ik3d :bind "set_deterministic"
  :hash 2586408642)
 :void (deterministic bool))

(defgmethod
 (iterate-ik3d+is-deterministic :class 'iterate-ik3d :bind "is_deterministic"
  :hash 36873697)
 bool)

(defgmethod
 (iterate-ik3d+set-target-node :class 'iterate-ik3d :bind "set_target_node"
  :hash 2761262315)
 :void (index int) (target-node node-path))

(defgmethod
 (iterate-ik3d+get-target-node :class 'iterate-ik3d :bind "get_target_node"
  :hash 408788394)
 node-path (index int))

(defgmethod
 (iterate-ik3d+set-joint-rotation-axis :class 'iterate-ik3d :bind
  "set_joint_rotation_axis" :hash 1391134969)
 :void (index int) (joint int) (axis skeleton-modifier-3d+rotation-axis))

(defgmethod
 (iterate-ik3d+get-joint-rotation-axis :class 'iterate-ik3d :bind
  "get_joint_rotation_axis" :hash 3312594080)
 skeleton-modifier-3d+rotation-axis (index int) (joint int))

(defgmethod
 (iterate-ik3d+set-joint-rotation-axis-vector :class 'iterate-ik3d :bind
  "set_joint_rotation_axis_vector" :hash 2866752138)
 :void (index int) (joint int) (axis-vector vector-3))

(defgmethod
 (iterate-ik3d+get-joint-rotation-axis-vector :class 'iterate-ik3d :bind
  "get_joint_rotation_axis_vector" :hash 1592972041)
 vector-3 (index int) (joint int))

(defgmethod
 (iterate-ik3d+set-joint-limitation :class 'iterate-ik3d :bind
  "set_joint_limitation" :hash 1194636955)
 :void (index int) (joint int) (limitation joint-limitation-3d))

(defgmethod
 (iterate-ik3d+get-joint-limitation :class 'iterate-ik3d :bind
  "get_joint_limitation" :hash 91665146)
 joint-limitation-3d (index int) (joint int))

(defgmethod
 (iterate-ik3d+set-joint-limitation-right-axis :class 'iterate-ik3d :bind
  "set_joint_limitation_right_axis" :hash 3838967147)
 :void (index int) (joint int)
 (direction skeleton-modifier-3d+secondary-direction))

(defgmethod
 (iterate-ik3d+get-joint-limitation-right-axis :class 'iterate-ik3d :bind
  "get_joint_limitation_right_axis" :hash 623936134)
 skeleton-modifier-3d+secondary-direction (index int) (joint int))

(defgmethod
 (iterate-ik3d+set-joint-limitation-right-axis-vector :class 'iterate-ik3d
  :bind "set_joint_limitation_right_axis_vector" :hash 2866752138)
 :void (index int) (joint int) (vector vector-3))

(defgmethod
 (iterate-ik3d+get-joint-limitation-right-axis-vector :class 'iterate-ik3d
  :bind "get_joint_limitation_right_axis_vector" :hash 1592972041)
 vector-3 (index int) (joint int))

(defgmethod
 (iterate-ik3d+set-joint-limitation-rotation-offset :class 'iterate-ik3d :bind
  "set_joint_limitation_rotation_offset" :hash 4188936002)
 :void (index int) (joint int) (offset quaternion))

(defgmethod
 (iterate-ik3d+get-joint-limitation-rotation-offset :class 'iterate-ik3d :bind
  "get_joint_limitation_rotation_offset" :hash 2722473700)
 quaternion (index int) (joint int))