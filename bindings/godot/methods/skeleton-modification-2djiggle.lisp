(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2djiggle+set-target-node :class
  'skeleton-modification-2djiggle :bind "set_target_node" :hash 1348162250)
 :void (target-nodepath node-path))

(defgmethod
 (skeleton-modification-2djiggle+get-target-node :class
  'skeleton-modification-2djiggle :bind "get_target_node" :hash 4075236667)
 node-path)

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-data-chain-length :class
  'skeleton-modification-2djiggle :bind "set_jiggle_data_chain_length" :hash
  1286410249)
 :void (length int))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-data-chain-length :class
  'skeleton-modification-2djiggle :bind "get_jiggle_data_chain_length" :hash
  2455072627)
 int)

(defgmethod
 (skeleton-modification-2djiggle+set-stiffness :class
  'skeleton-modification-2djiggle :bind "set_stiffness" :hash 373806689)
 :void (stiffness float))

(defgmethod
 (skeleton-modification-2djiggle+get-stiffness :class
  'skeleton-modification-2djiggle :bind "get_stiffness" :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-2djiggle+set-mass :class
  'skeleton-modification-2djiggle :bind "set_mass" :hash 373806689)
 :void (mass float))

(defgmethod
 (skeleton-modification-2djiggle+get-mass :class
  'skeleton-modification-2djiggle :bind "get_mass" :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-2djiggle+set-damping :class
  'skeleton-modification-2djiggle :bind "set_damping" :hash 373806689)
 :void (damping float))

(defgmethod
 (skeleton-modification-2djiggle+get-damping :class
  'skeleton-modification-2djiggle :bind "get_damping" :hash 1740695150)
 float)

(defgmethod
 (skeleton-modification-2djiggle+set-use-gravity :class
  'skeleton-modification-2djiggle :bind "set_use_gravity" :hash 2586408642)
 :void (use-gravity bool))

(defgmethod
 (skeleton-modification-2djiggle+get-use-gravity :class
  'skeleton-modification-2djiggle :bind "get_use_gravity" :hash 36873697)
 bool)

(defgmethod
 (skeleton-modification-2djiggle+set-gravity :class
  'skeleton-modification-2djiggle :bind "set_gravity" :hash 743155724)
 :void (gravity vector-2))

(defgmethod
 (skeleton-modification-2djiggle+get-gravity :class
  'skeleton-modification-2djiggle :bind "get_gravity" :hash 3341600327)
 vector-2)

(defgmethod
 (skeleton-modification-2djiggle+set-use-colliders :class
  'skeleton-modification-2djiggle :bind "set_use_colliders" :hash 2586408642)
 :void (use-colliders bool))

(defgmethod
 (skeleton-modification-2djiggle+get-use-colliders :class
  'skeleton-modification-2djiggle :bind "get_use_colliders" :hash 36873697)
 bool)

(defgmethod
 (skeleton-modification-2djiggle+set-collision-mask :class
  'skeleton-modification-2djiggle :bind "set_collision_mask" :hash 1286410249)
 :void (collision-mask int))

(defgmethod
 (skeleton-modification-2djiggle+get-collision-mask :class
  'skeleton-modification-2djiggle :bind "get_collision_mask" :hash 3905245786)
 int)

(defgmethod
 (skeleton-modification-2djiggle+reset :class 'skeleton-modification-2djiggle
  :bind "reset" :hash 3218959716)
 :void)

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-bone2d-node :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_bone2d_node" :hash
  2761262315)
 :void (joint-idx int) (bone2d-node node-path))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-bone2d-node :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_bone2d_node" :hash
  408788394)
 node-path (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-bone-index :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_bone_index" :hash
  3937882851)
 :void (joint-idx int) (bone-idx int))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-bone-index :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_bone_index" :hash
  923996154)
 int (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-override :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_override" :hash
  300928843)
 :void (joint-idx int) (override bool))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-override :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_override" :hash
  1116898809)
 bool (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-stiffness :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_stiffness" :hash
  1602489585)
 :void (joint-idx int) (stiffness float))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-stiffness :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_stiffness" :hash
  2339986948)
 float (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-mass :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_mass" :hash
  1602489585)
 :void (joint-idx int) (mass float))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-mass :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_mass" :hash
  2339986948)
 float (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-damping :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_damping" :hash
  1602489585)
 :void (joint-idx int) (damping float))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-damping :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_damping" :hash
  2339986948)
 float (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-use-gravity :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_use_gravity" :hash
  300928843)
 :void (joint-idx int) (use-gravity bool))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-use-gravity :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_use_gravity" :hash
  1116898809)
 bool (joint-idx int))

(defgmethod
 (skeleton-modification-2djiggle+set-jiggle-joint-gravity :class
  'skeleton-modification-2djiggle :bind "set_jiggle_joint_gravity" :hash
  163021252)
 :void (joint-idx int) (gravity vector-2))

(defgmethod
 (skeleton-modification-2djiggle+get-jiggle-joint-gravity :class
  'skeleton-modification-2djiggle :bind "get_jiggle_joint_gravity" :hash
  2299179447)
 vector-2 (joint-idx int))