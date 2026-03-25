(common-lisp:in-package :%godot)


(defgmethod
 (chain-ik3d+set-root-bone-name :class 'chain-ik3d :bind "set_root_bone_name"
  :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (chain-ik3d+get-root-bone-name :class 'chain-ik3d :bind "get_root_bone_name"
  :hash 844755477)
 string (index int))

(defgmethod
 (chain-ik3d+set-root-bone :class 'chain-ik3d :bind "set_root_bone" :hash
  3937882851)
 :void (index int) (bone int))

(defgmethod
 (chain-ik3d+get-root-bone :class 'chain-ik3d :bind "get_root_bone" :hash
  923996154)
 int (index int))

(defgmethod
 (chain-ik3d+set-end-bone-name :class 'chain-ik3d :bind "set_end_bone_name"
  :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (chain-ik3d+get-end-bone-name :class 'chain-ik3d :bind "get_end_bone_name"
  :hash 844755477)
 string (index int))

(defgmethod
 (chain-ik3d+set-end-bone :class 'chain-ik3d :bind "set_end_bone" :hash
  3937882851)
 :void (index int) (bone int))

(defgmethod
 (chain-ik3d+get-end-bone :class 'chain-ik3d :bind "get_end_bone" :hash
  923996154)
 int (index int))

(defgmethod
 (chain-ik3d+set-extend-end-bone :class 'chain-ik3d :bind "set_extend_end_bone"
  :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (chain-ik3d+is-end-bone-extended :class 'chain-ik3d :bind
  "is_end_bone_extended" :hash 1116898809)
 bool (index int))

(defgmethod
 (chain-ik3d+set-end-bone-direction :class 'chain-ik3d :bind
  "set_end_bone_direction" :hash 2838484201)
 :void (index int) (bone-direction skeleton-modifier-3d+bone-direction))

(defgmethod
 (chain-ik3d+get-end-bone-direction :class 'chain-ik3d :bind
  "get_end_bone_direction" :hash 1843036459)
 skeleton-modifier-3d+bone-direction (index int))

(defgmethod
 (chain-ik3d+set-end-bone-length :class 'chain-ik3d :bind "set_end_bone_length"
  :hash 1602489585)
 :void (index int) (length float))

(defgmethod
 (chain-ik3d+get-end-bone-length :class 'chain-ik3d :bind "get_end_bone_length"
  :hash 2339986948)
 float (index int))

(defgmethod
 (chain-ik3d+get-joint-bone-name :class 'chain-ik3d :bind "get_joint_bone_name"
  :hash 1391810591)
 string (index int) (joint int))

(defgmethod
 (chain-ik3d+get-joint-bone :class 'chain-ik3d :bind "get_joint_bone" :hash
  3175239445)
 int (index int) (joint int))

(defgmethod
 (chain-ik3d+get-joint-count :class 'chain-ik3d :bind "get_joint_count" :hash
  923996154)
 int (index int))