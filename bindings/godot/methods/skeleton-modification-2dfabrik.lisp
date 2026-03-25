(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dfabrik+set-target-node :class
  'skeleton-modification-2dfabrik :bind "set_target_node" :hash 1348162250)
 :void (target-nodepath node-path))

(defgmethod
 (skeleton-modification-2dfabrik+get-target-node :class
  'skeleton-modification-2dfabrik :bind "get_target_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2dfabrik+set-fabrik-data-chain-length :class
  'skeleton-modification-2dfabrik :bind "set_fabrik_data_chain_length" :hash
  1286410249)
 :void (length int))

(defgmethod
 (skeleton-modification-2dfabrik+get-fabrik-data-chain-length :class
  'skeleton-modification-2dfabrik :bind "get_fabrik_data_chain_length" :hash
  2455072627)
 int)

(defgmethod
 (skeleton-modification-2dfabrik+set-fabrik-joint-bone2d-node :class
  'skeleton-modification-2dfabrik :bind "set_fabrik_joint_bone2d_node" :hash
  2761262315)
 :void (joint-idx int) (bone2d-nodepath node-path))

(defgmethod
 (skeleton-modification-2dfabrik+get-fabrik-joint-bone2d-node :class
  'skeleton-modification-2dfabrik :bind "get_fabrik_joint_bone2d_node" :hash
  408788394)
 node-path (joint-idx int))

(defgmethod
 (skeleton-modification-2dfabrik+set-fabrik-joint-bone-index :class
  'skeleton-modification-2dfabrik :bind "set_fabrik_joint_bone_index" :hash
  3937882851)
 :void (joint-idx int) (bone-idx int))

(defgmethod
 (skeleton-modification-2dfabrik+get-fabrik-joint-bone-index :class
  'skeleton-modification-2dfabrik :bind "get_fabrik_joint_bone_index" :hash
  923996154)
 int (joint-idx int))

(defgmethod
 (skeleton-modification-2dfabrik+set-fabrik-joint-magnet-position :class
  'skeleton-modification-2dfabrik :bind "set_fabrik_joint_magnet_position"
  :hash 163021252)
 :void (joint-idx int) (magnet-position vector-2))

(defgmethod
 (skeleton-modification-2dfabrik+get-fabrik-joint-magnet-position :class
  'skeleton-modification-2dfabrik :bind "get_fabrik_joint_magnet_position"
  :hash 2299179447)
 vector-2 (joint-idx int))

(defgmethod
 (skeleton-modification-2dfabrik+set-fabrik-joint-use-target-rotation :class
  'skeleton-modification-2dfabrik :bind "set_fabrik_joint_use_target_rotation"
  :hash 300928843)
 :void (joint-idx int) (use-target-rotation bool))

(defgmethod
 (skeleton-modification-2dfabrik+get-fabrik-joint-use-target-rotation :class
  'skeleton-modification-2dfabrik :bind "get_fabrik_joint_use_target_rotation"
  :hash 1116898809)
 bool (joint-idx int))