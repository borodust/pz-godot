(common-lisp:in-package :%godot)


(defgmethod
 (open-xrhand+set-hand :class 'open-xrhand :bind "set_hand" :hash 1849328560)
 :void (hand open-xrhand+hands))

(defgmethod
 (open-xrhand+get-hand :class 'open-xrhand :bind "get_hand" :hash 2850644561)
 open-xrhand+hands)

(defgmethod
 (open-xrhand+set-hand-skeleton :class 'open-xrhand :bind "set_hand_skeleton"
  :hash 1348162250)
 :void (hand-skeleton node-path))

(defgmethod
 (open-xrhand+get-hand-skeleton :class 'open-xrhand :bind "get_hand_skeleton"
  :hash 4075236667)
 node-path)

(defgmethod
 (open-xrhand+set-motion-range :class 'open-xrhand :bind "set_motion_range"
  :hash 3326516003)
 :void (motion-range open-xrhand+motion-range))

(defgmethod
 (open-xrhand+get-motion-range :class 'open-xrhand :bind "get_motion_range"
  :hash 2191822314)
 open-xrhand+motion-range)

(defgmethod
 (open-xrhand+set-skeleton-rig :class 'open-xrhand :bind "set_skeleton_rig"
  :hash 1528072213)
 :void (skeleton-rig open-xrhand+skeleton-rig))

(defgmethod
 (open-xrhand+get-skeleton-rig :class 'open-xrhand :bind "get_skeleton_rig"
  :hash 968409338)
 open-xrhand+skeleton-rig)

(defgmethod
 (open-xrhand+set-bone-update :class 'open-xrhand :bind "set_bone_update" :hash
  3144625444)
 :void (bone-update open-xrhand+bone-update))

(defgmethod
 (open-xrhand+get-bone-update :class 'open-xrhand :bind "get_bone_update" :hash
  1310695248)
 open-xrhand+bone-update)