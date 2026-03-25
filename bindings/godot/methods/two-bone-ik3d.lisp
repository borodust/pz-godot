(common-lisp:in-package :%godot)


(defgmethod
 (two-bone-ik3d+set-target-node :class 'two-bone-ik3d :bind "set_target_node"
  :hash 2761262315)
 :void (index int) (target-node node-path))

(defgmethod
 (two-bone-ik3d+get-target-node :class 'two-bone-ik3d :bind "get_target_node"
  :hash 408788394)
 node-path (index int))

(defgmethod
 (two-bone-ik3d+set-pole-node :class 'two-bone-ik3d :bind "set_pole_node" :hash
  2761262315)
 :void (index int) (pole-node node-path))

(defgmethod
 (two-bone-ik3d+get-pole-node :class 'two-bone-ik3d :bind "get_pole_node" :hash
  408788394)
 node-path (index int))

(defgmethod
 (two-bone-ik3d+set-root-bone-name :class 'two-bone-ik3d :bind
  "set_root_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (two-bone-ik3d+get-root-bone-name :class 'two-bone-ik3d :bind
  "get_root_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (two-bone-ik3d+set-root-bone :class 'two-bone-ik3d :bind "set_root_bone" :hash
  3937882851)
 :void (index int) (bone int))

(defgmethod
 (two-bone-ik3d+get-root-bone :class 'two-bone-ik3d :bind "get_root_bone" :hash
  923996154)
 int (index int))

(defgmethod
 (two-bone-ik3d+set-middle-bone-name :class 'two-bone-ik3d :bind
  "set_middle_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (two-bone-ik3d+get-middle-bone-name :class 'two-bone-ik3d :bind
  "get_middle_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (two-bone-ik3d+set-middle-bone :class 'two-bone-ik3d :bind "set_middle_bone"
  :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (two-bone-ik3d+get-middle-bone :class 'two-bone-ik3d :bind "get_middle_bone"
  :hash 923996154)
 int (index int))

(defgmethod
 (two-bone-ik3d+set-pole-direction :class 'two-bone-ik3d :bind
  "set_pole_direction" :hash 258741388)
 :void (index int) (direction skeleton-modifier-3d+secondary-direction))

(defgmethod
 (two-bone-ik3d+get-pole-direction :class 'two-bone-ik3d :bind
  "get_pole_direction" :hash 377522128)
 skeleton-modifier-3d+secondary-direction (index int))

(defgmethod
 (two-bone-ik3d+set-pole-direction-vector :class 'two-bone-ik3d :bind
  "set_pole_direction_vector" :hash 1530502735)
 :void (index int) (vector vector-3))

(defgmethod
 (two-bone-ik3d+get-pole-direction-vector :class 'two-bone-ik3d :bind
  "get_pole_direction_vector" :hash 711720468)
 vector-3 (index int))

(defgmethod
 (two-bone-ik3d+set-end-bone-name :class 'two-bone-ik3d :bind
  "set_end_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (two-bone-ik3d+get-end-bone-name :class 'two-bone-ik3d :bind
  "get_end_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (two-bone-ik3d+set-end-bone :class 'two-bone-ik3d :bind "set_end_bone" :hash
  3937882851)
 :void (index int) (bone int))

(defgmethod
 (two-bone-ik3d+get-end-bone :class 'two-bone-ik3d :bind "get_end_bone" :hash
  923996154)
 int (index int))

(defgmethod
 (two-bone-ik3d+set-use-virtual-end :class 'two-bone-ik3d :bind
  "set_use_virtual_end" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (two-bone-ik3d+is-using-virtual-end :class 'two-bone-ik3d :bind
  "is_using_virtual_end" :hash 1116898809)
 bool (index int))

(defgmethod
 (two-bone-ik3d+set-extend-end-bone :class 'two-bone-ik3d :bind
  "set_extend_end_bone" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (two-bone-ik3d+is-end-bone-extended :class 'two-bone-ik3d :bind
  "is_end_bone_extended" :hash 1116898809)
 bool (index int))

(defgmethod
 (two-bone-ik3d+set-end-bone-direction :class 'two-bone-ik3d :bind
  "set_end_bone_direction" :hash 2838484201)
 :void (index int) (bone-direction skeleton-modifier-3d+bone-direction))

(defgmethod
 (two-bone-ik3d+get-end-bone-direction :class 'two-bone-ik3d :bind
  "get_end_bone_direction" :hash 1843036459)
 skeleton-modifier-3d+bone-direction (index int))

(defgmethod
 (two-bone-ik3d+set-end-bone-length :class 'two-bone-ik3d :bind
  "set_end_bone_length" :hash 1602489585)
 :void (index int) (length float))

(defgmethod
 (two-bone-ik3d+get-end-bone-length :class 'two-bone-ik3d :bind
  "get_end_bone_length" :hash 2339986948)
 float (index int))