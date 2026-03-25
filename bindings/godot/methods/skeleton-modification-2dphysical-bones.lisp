(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-modification-2dphysical-bones+set-physical-bone-chain-length :class
  'skeleton-modification-2dphysical-bones :bind
  "set_physical_bone_chain_length" :hash 1286410249)
 :void (length int))

(defgmethod
 (skeleton-modification-2dphysical-bones+get-physical-bone-chain-length :class
  'skeleton-modification-2dphysical-bones :bind
  "get_physical_bone_chain_length" :hash 2455072627)
 int)

(defgmethod
 (skeleton-modification-2dphysical-bones+set-physical-bone-node :class
  'skeleton-modification-2dphysical-bones :bind "set_physical_bone_node" :hash
  2761262315)
 :void (joint-idx int) (physicalbone2d-node node-path))

(defgmethod
 (skeleton-modification-2dphysical-bones+get-physical-bone-node :class
  'skeleton-modification-2dphysical-bones :bind "get_physical_bone_node" :hash
  408788394)
 node-path (joint-idx int))

(defgmethod
 (skeleton-modification-2dphysical-bones+fetch-physical-bones :class
  'skeleton-modification-2dphysical-bones :bind "fetch_physical_bones" :hash
  3218959716)
 :void)

(defgmethod
 (skeleton-modification-2dphysical-bones+start-simulation :class
  'skeleton-modification-2dphysical-bones :bind "start_simulation" :hash
  2787316981)
 :void (bones array))

(defgmethod
 (skeleton-modification-2dphysical-bones+stop-simulation :class
  'skeleton-modification-2dphysical-bones :bind "stop_simulation" :hash
  2787316981)
 :void (bones array))