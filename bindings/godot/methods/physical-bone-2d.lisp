(common-lisp:in-package :%godot)


(defgmethod
 (physical-bone-2d+get-joint :class 'physical-bone-2d :bind "get_joint" :hash
  3582132112)
 joint-2d)

(defgmethod
 (physical-bone-2d+get-auto-configure-joint :class 'physical-bone-2d :bind
  "get_auto_configure_joint" :hash 36873697)
 bool)

(defgmethod
 (physical-bone-2d+set-auto-configure-joint :class 'physical-bone-2d :bind
  "set_auto_configure_joint" :hash 2586408642)
 :void (auto-configure-joint bool))

(defgmethod
 (physical-bone-2d+set-simulate-physics :class 'physical-bone-2d :bind
  "set_simulate_physics" :hash 2586408642)
 :void (simulate-physics bool))

(defgmethod
 (physical-bone-2d+get-simulate-physics :class 'physical-bone-2d :bind
  "get_simulate_physics" :hash 36873697)
 bool)

(defgmethod
 (physical-bone-2d+is-simulating-physics :class 'physical-bone-2d :bind
  "is_simulating_physics" :hash 36873697)
 bool)

(defgmethod
 (physical-bone-2d+set-bone2d-nodepath :class 'physical-bone-2d :bind
  "set_bone2d_nodepath" :hash 1348162250)
 :void (nodepath node-path))

(defgmethod
 (physical-bone-2d+get-bone2d-nodepath :class 'physical-bone-2d :bind
  "get_bone2d_nodepath" :hash 4075236667)
 node-path)

(defgmethod
 (physical-bone-2d+set-bone2d-index :class 'physical-bone-2d :bind
  "set_bone2d_index" :hash 1286410249)
 :void (bone-index int))

(defgmethod
 (physical-bone-2d+get-bone2d-index :class 'physical-bone-2d :bind
  "get_bone2d_index" :hash 3905245786)
 int)

(defgmethod
 (physical-bone-2d+set-follow-bone-when-simulating :class 'physical-bone-2d
  :bind "set_follow_bone_when_simulating" :hash 2586408642)
 :void (follow-bone bool))

(defgmethod
 (physical-bone-2d+get-follow-bone-when-simulating :class 'physical-bone-2d
  :bind "get_follow_bone_when_simulating" :hash 36873697)
 bool)