(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-target-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_target_node" :hash
  1348162250)
 :void (target-nodepath node-path))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-target-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_target_node" :hash
  4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-target-minimum-distance :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_target_minimum_distance"
  :hash 373806689)
 :void (minimum-distance float))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-target-minimum-distance :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_target_minimum_distance"
  :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-target-maximum-distance :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_target_maximum_distance"
  :hash 373806689)
 :void (maximum-distance float))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-target-maximum-distance :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_target_maximum_distance"
  :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-flip-bend-direction :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_flip_bend_direction" :hash
  2586408642)
 :void (flip-direction bool))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-flip-bend-direction :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_flip_bend_direction" :hash
  36873697)
 bool)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-joint-one-bone2d-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_joint_one_bone2d_node" :hash
  1348162250)
 :void (bone2d-node node-path))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-joint-one-bone2d-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_joint_one_bone2d_node" :hash
  4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-joint-one-bone-idx :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_joint_one_bone_idx" :hash
  1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-joint-one-bone-idx :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_joint_one_bone_idx" :hash
  3905245786)
 int)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-joint-two-bone2d-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_joint_two_bone2d_node" :hash
  1348162250)
 :void (bone2d-node node-path))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-joint-two-bone2d-node :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_joint_two_bone2d_node" :hash
  4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+set-joint-two-bone-idx :class
  'skeleton-modification-2dtwo-bone-ik :bind "set_joint_two_bone_idx" :hash
  1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-modification-2dtwo-bone-ik+get-joint-two-bone-idx :class
  'skeleton-modification-2dtwo-bone-ik :bind "get_joint_two_bone_idx" :hash
  3905245786)
 int)