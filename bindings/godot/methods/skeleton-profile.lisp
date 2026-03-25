(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-profile+set-root-bone :class 'skeleton-profile :bind "set_root_bone"
  :hash 3304788590)
 :void (bone-name string-name))

(defgmethod
 (skeleton-profile+get-root-bone :class 'skeleton-profile :bind "get_root_bone"
  :hash 2737447660)
 string-name)

(defgmethod
 (skeleton-profile+set-scale-base-bone :class 'skeleton-profile :bind
  "set_scale_base_bone" :hash 3304788590)
 :void (bone-name string-name))

(defgmethod
 (skeleton-profile+get-scale-base-bone :class 'skeleton-profile :bind
  "get_scale_base_bone" :hash 2737447660)
 string-name)

(defgmethod
 (skeleton-profile+set-group-size :class 'skeleton-profile :bind
  "set_group_size" :hash 1286410249)
 :void (size int))

(defgmethod
 (skeleton-profile+get-group-size :class 'skeleton-profile :bind
  "get_group_size" :hash 2455072627)
 int)

(defgmethod
 (skeleton-profile+get-group-name :class 'skeleton-profile :bind
  "get_group_name" :hash 659327637)
 string-name (group-idx int))

(defgmethod
 (skeleton-profile+set-group-name :class 'skeleton-profile :bind
  "set_group_name" :hash 3780747571)
 :void (group-idx int) (group-name string-name))

(defgmethod
 (skeleton-profile+get-texture :class 'skeleton-profile :bind "get_texture"
  :hash 3536238170)
 texture-2d (group-idx int))

(defgmethod
 (skeleton-profile+set-texture :class 'skeleton-profile :bind "set_texture"
  :hash 666127730)
 :void (group-idx int) (texture texture-2d))

(defgmethod
 (skeleton-profile+set-bone-size :class 'skeleton-profile :bind "set_bone_size"
  :hash 1286410249)
 :void (size int))

(defgmethod
 (skeleton-profile+get-bone-size :class 'skeleton-profile :bind "get_bone_size"
  :hash 2455072627)
 int)

(defgmethod
 (skeleton-profile+find-bone :class 'skeleton-profile :bind "find_bone" :hash
  2458036349)
 int (bone-name string-name))

(defgmethod
 (skeleton-profile+get-bone-name :class 'skeleton-profile :bind "get_bone_name"
  :hash 659327637)
 string-name (bone-idx int))

(defgmethod
 (skeleton-profile+set-bone-name :class 'skeleton-profile :bind "set_bone_name"
  :hash 3780747571)
 :void (bone-idx int) (bone-name string-name))

(defgmethod
 (skeleton-profile+get-bone-parent :class 'skeleton-profile :bind
  "get_bone_parent" :hash 659327637)
 string-name (bone-idx int))

(defgmethod
 (skeleton-profile+set-bone-parent :class 'skeleton-profile :bind
  "set_bone_parent" :hash 3780747571)
 :void (bone-idx int) (bone-parent string-name))

(defgmethod
 (skeleton-profile+get-tail-direction :class 'skeleton-profile :bind
  "get_tail_direction" :hash 2675997574)
 skeleton-profile+tail-direction (bone-idx int))

(defgmethod
 (skeleton-profile+set-tail-direction :class 'skeleton-profile :bind
  "set_tail_direction" :hash 1231951015)
 :void (bone-idx int) (tail-direction skeleton-profile+tail-direction))

(defgmethod
 (skeleton-profile+get-bone-tail :class 'skeleton-profile :bind "get_bone_tail"
  :hash 659327637)
 string-name (bone-idx int))

(defgmethod
 (skeleton-profile+set-bone-tail :class 'skeleton-profile :bind "set_bone_tail"
  :hash 3780747571)
 :void (bone-idx int) (bone-tail string-name))

(defgmethod
 (skeleton-profile+get-reference-pose :class 'skeleton-profile :bind
  "get_reference_pose" :hash 1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-profile+set-reference-pose :class 'skeleton-profile :bind
  "set_reference_pose" :hash 3616898986)
 :void (bone-idx int) (bone-name transform-3d))

(defgmethod
 (skeleton-profile+get-handle-offset :class 'skeleton-profile :bind
  "get_handle_offset" :hash 2299179447)
 vector-2 (bone-idx int))

(defgmethod
 (skeleton-profile+set-handle-offset :class 'skeleton-profile :bind
  "set_handle_offset" :hash 163021252)
 :void (bone-idx int) (handle-offset vector-2))

(defgmethod
 (skeleton-profile+get-group :class 'skeleton-profile :bind "get_group" :hash
  659327637)
 string-name (bone-idx int))

(defgmethod
 (skeleton-profile+set-group :class 'skeleton-profile :bind "set_group" :hash
  3780747571)
 :void (bone-idx int) (group string-name))

(defgmethod
 (skeleton-profile+is-required :class 'skeleton-profile :bind "is_required"
  :hash 1116898809)
 bool (bone-idx int))

(defgmethod
 (skeleton-profile+set-required :class 'skeleton-profile :bind "set_required"
  :hash 300928843)
 :void (bone-idx int) (required bool))