(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dccdik+set-target-node :class
  'skeleton-modification-2dccdik :bind "set_target_node" :hash 1348162250)
 :void (target-nodepath node-path))

(defgmethod
 (skeleton-modification-2dccdik+get-target-node :class
  'skeleton-modification-2dccdik :bind "get_target_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dccdik+set-tip-node :class
  'skeleton-modification-2dccdik :bind "set_tip_node" :hash 1348162250)
 :void (tip-nodepath node-path))

(defgmethod
 (skeleton-modification-2dccdik+get-tip-node :class
  'skeleton-modification-2dccdik :bind "get_tip_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-data-chain-length :class
  'skeleton-modification-2dccdik :bind "set_ccdik_data_chain_length" :hash
  1286410249)
 :void (length int))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-data-chain-length :class
  'skeleton-modification-2dccdik :bind "get_ccdik_data_chain_length" :hash
  2455072627)
 int)

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-bone2d-node :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_bone2d_node" :hash
  2761262315)
 :void (joint-idx int) (bone2d-nodepath node-path))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-bone2d-node :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_bone2d_node" :hash
  408788394)
 node-path (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-bone-index :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_bone_index" :hash
  3937882851)
 :void (joint-idx int) (bone-idx int))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-bone-index :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_bone_index" :hash
  923996154)
 int (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-rotate-from-joint :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_rotate_from_joint"
  :hash 300928843)
 :void (joint-idx int) (rotate-from-joint bool))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-rotate-from-joint :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_rotate_from_joint"
  :hash 1116898809)
 bool (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-enable-constraint :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_enable_constraint"
  :hash 300928843)
 :void (joint-idx int) (enable-constraint bool))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-enable-constraint :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_enable_constraint"
  :hash 1116898809)
 bool (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-constraint-angle-min :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_constraint_angle_min"
  :hash 1602489585)
 :void (joint-idx int) (angle-min float))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-constraint-angle-min :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_constraint_angle_min"
  :hash 2339986948)
 float (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-constraint-angle-max :class
  'skeleton-modification-2dccdik :bind "set_ccdik_joint_constraint_angle_max"
  :hash 1602489585)
 :void (joint-idx int) (angle-max float))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-constraint-angle-max :class
  'skeleton-modification-2dccdik :bind "get_ccdik_joint_constraint_angle_max"
  :hash 2339986948)
 float (joint-idx int))

(defgmethod
 (skeleton-modification-2dccdik+set-ccdik-joint-constraint-angle-invert :class
  'skeleton-modification-2dccdik :bind
  "set_ccdik_joint_constraint_angle_invert" :hash 300928843)
 :void (joint-idx int) (invert bool))

(defgmethod
 (skeleton-modification-2dccdik+get-ccdik-joint-constraint-angle-invert :class
  'skeleton-modification-2dccdik :bind
  "get_ccdik_joint_constraint_angle_invert" :hash 1116898809)
 bool (joint-idx int))