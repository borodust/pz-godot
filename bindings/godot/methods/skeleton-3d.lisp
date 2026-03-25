(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-3d+add-bone :class 'skeleton-3d :bind "add_bone" :hash 1597066294)
 int (name string))

(defgmethod
 (skeleton-3d+find-bone :class 'skeleton-3d :bind "find_bone" :hash 1321353865)
 int (name string))

(defgmethod
 (skeleton-3d+get-bone-name :class 'skeleton-3d :bind "get_bone_name" :hash
  844755477)
 string (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-name :class 'skeleton-3d :bind "set_bone_name" :hash
  501894301)
 :void (bone-idx int) (name string))

(defgmethod
 (skeleton-3d+get-bone-meta :class 'skeleton-3d :bind "get_bone_meta" :hash
  203112058)
 variant (bone-idx int) (key string-name))

(defgmethod
 (skeleton-3d+get-bone-meta-list :class 'skeleton-3d :bind "get_bone_meta_list"
  :hash 663333327)
 array (bone-idx int))

(defgmethod
 (skeleton-3d+has-bone-meta :class 'skeleton-3d :bind "has_bone_meta" :hash
  921227809)
 bool (bone-idx int) (key string-name))

(defgmethod
 (skeleton-3d+set-bone-meta :class 'skeleton-3d :bind "set_bone_meta" :hash
  702482756)
 :void (bone-idx int) (key string-name) (value variant))

(defgmethod
 (skeleton-3d+get-concatenated-bone-names :class 'skeleton-3d :bind
  "get_concatenated_bone_names" :hash 2002593661)
 string-name)

(defgmethod
 (skeleton-3d+get-bone-parent :class 'skeleton-3d :bind "get_bone_parent" :hash
  923996154)
 int (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-parent :class 'skeleton-3d :bind "set_bone_parent" :hash
  3937882851)
 :void (bone-idx int) (parent-idx int))

(defgmethod
 (skeleton-3d+get-bone-count :class 'skeleton-3d :bind "get_bone_count" :hash
  3905245786)
 int)

(defgmethod
 (skeleton-3d+get-version :class 'skeleton-3d :bind "get_version" :hash
  3905245786)
 int)

(defgmethod
 (skeleton-3d+unparent-bone-and-rest :class 'skeleton-3d :bind
  "unparent_bone_and_rest" :hash 1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-3d+get-bone-children :class 'skeleton-3d :bind "get_bone_children"
  :hash 1706082319)
 packed-int-32array (bone-idx int))

(defgmethod
 (skeleton-3d+get-parentless-bones :class 'skeleton-3d :bind
  "get_parentless_bones" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (skeleton-3d+get-bone-rest :class 'skeleton-3d :bind "get_bone_rest" :hash
  1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-rest :class 'skeleton-3d :bind "set_bone_rest" :hash
  3616898986)
 :void (bone-idx int) (rest transform-3d))

(defgmethod
 (skeleton-3d+get-bone-global-rest :class 'skeleton-3d :bind
  "get_bone_global_rest" :hash 1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+create-skin-from-rest-transforms :class 'skeleton-3d :bind
  "create_skin_from_rest_transforms" :hash 1032037385)
 skin)

(defgmethod
 (skeleton-3d+register-skin :class 'skeleton-3d :bind "register_skin" :hash
  3405789568)
 skin-reference (skin skin))

(defgmethod
 (skeleton-3d+localize-rests :class 'skeleton-3d :bind "localize_rests" :hash
  3218959716)
 :void)

(defgmethod
 (skeleton-3d+clear-bones :class 'skeleton-3d :bind "clear_bones" :hash
  3218959716)
 :void)

(defgmethod
 (skeleton-3d+get-bone-pose :class 'skeleton-3d :bind "get_bone_pose" :hash
  1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-pose :class 'skeleton-3d :bind "set_bone_pose" :hash
  3616898986)
 :void (bone-idx int) (pose transform-3d))

(defgmethod
 (skeleton-3d+set-bone-pose-position :class 'skeleton-3d :bind
  "set_bone_pose_position" :hash 1530502735)
 :void (bone-idx int) (position vector-3))

(defgmethod
 (skeleton-3d+set-bone-pose-rotation :class 'skeleton-3d :bind
  "set_bone_pose_rotation" :hash 2823819782)
 :void (bone-idx int) (rotation quaternion))

(defgmethod
 (skeleton-3d+set-bone-pose-scale :class 'skeleton-3d :bind
  "set_bone_pose_scale" :hash 1530502735)
 :void (bone-idx int) (scale vector-3))

(defgmethod
 (skeleton-3d+get-bone-pose-position :class 'skeleton-3d :bind
  "get_bone_pose_position" :hash 711720468)
 vector-3 (bone-idx int))

(defgmethod
 (skeleton-3d+get-bone-pose-rotation :class 'skeleton-3d :bind
  "get_bone_pose_rotation" :hash 476865136)
 quaternion (bone-idx int))

(defgmethod
 (skeleton-3d+get-bone-pose-scale :class 'skeleton-3d :bind
  "get_bone_pose_scale" :hash 711720468)
 vector-3 (bone-idx int))

(defgmethod
 (skeleton-3d+reset-bone-pose :class 'skeleton-3d :bind "reset_bone_pose" :hash
  1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-3d+reset-bone-poses :class 'skeleton-3d :bind "reset_bone_poses"
  :hash 3218959716)
 :void)

(defgmethod
 (skeleton-3d+is-bone-enabled :class 'skeleton-3d :bind "is_bone_enabled" :hash
  1116898809)
 bool (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-enabled :class 'skeleton-3d :bind "set_bone_enabled"
  :hash 972357352)
 :void (bone-idx int) (enabled bool))

(defgmethod
 (skeleton-3d+get-bone-global-pose :class 'skeleton-3d :bind
  "get_bone_global_pose" :hash 1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+set-bone-global-pose :class 'skeleton-3d :bind
  "set_bone_global_pose" :hash 3616898986)
 :void (bone-idx int) (pose transform-3d))

(defgmethod
 (skeleton-3d+force-update-all-bone-transforms :class 'skeleton-3d :bind
  "force_update_all_bone_transforms" :hash 3218959716)
 :void)

(defgmethod
 (skeleton-3d+force-update-bone-child-transform :class 'skeleton-3d :bind
  "force_update_bone_child_transform" :hash 1286410249)
 :void (bone-idx int))

(defgmethod
 (skeleton-3d+set-motion-scale :class 'skeleton-3d :bind "set_motion_scale"
  :hash 373806689)
 :void (motion-scale float))

(defgmethod
 (skeleton-3d+get-motion-scale :class 'skeleton-3d :bind "get_motion_scale"
  :hash 1740695150)
 float)

(defgmethod
 (skeleton-3d+set-show-rest-only :class 'skeleton-3d :bind "set_show_rest_only"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (skeleton-3d+is-show-rest-only :class 'skeleton-3d :bind "is_show_rest_only"
  :hash 36873697)
 bool)

(defgmethod
 (skeleton-3d+set-modifier-callback-mode-process :class 'skeleton-3d :bind
  "set_modifier_callback_mode_process" :hash 3916362634)
 :void (mode skeleton-3d+modifier-callback-mode-process))

(defgmethod
 (skeleton-3d+get-modifier-callback-mode-process :class 'skeleton-3d :bind
  "get_modifier_callback_mode_process" :hash 997182536)
 skeleton-3d+modifier-callback-mode-process)

(defgmethod
 (skeleton-3d+advance :class 'skeleton-3d :bind "advance" :hash 373806689)
 :void (delta float))

(defgmethod
 (skeleton-3d+clear-bones-global-pose-override :class 'skeleton-3d :bind
  "clear_bones_global_pose_override" :hash 3218959716)
 :void)

(defgmethod
 (skeleton-3d+set-bone-global-pose-override :class 'skeleton-3d :bind
  "set_bone_global_pose_override" :hash 3483398371)
 :void (bone-idx int) (pose transform-3d) (amount float) (persistent bool))

(defgmethod
 (skeleton-3d+get-bone-global-pose-override :class 'skeleton-3d :bind
  "get_bone_global_pose_override" :hash 1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+get-bone-global-pose-no-override :class 'skeleton-3d :bind
  "get_bone_global_pose_no_override" :hash 1965739696)
 transform-3d (bone-idx int))

(defgmethod
 (skeleton-3d+set-animate-physical-bones :class 'skeleton-3d :bind
  "set_animate_physical_bones" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (skeleton-3d+get-animate-physical-bones :class 'skeleton-3d :bind
  "get_animate_physical_bones" :hash 36873697)
 bool)

(defgmethod
 (skeleton-3d+physical-bones-stop-simulation :class 'skeleton-3d :bind
  "physical_bones_stop_simulation" :hash 3218959716)
 :void)

(defgmethod
 (skeleton-3d+physical-bones-start-simulation :class 'skeleton-3d :bind
  "physical_bones_start_simulation" :hash 2787316981)
 :void (bones array))

(defgmethod
 (skeleton-3d+physical-bones-add-collision-exception :class 'skeleton-3d :bind
  "physical_bones_add_collision_exception" :hash 2722037293)
 :void (exception rid))

(defgmethod
 (skeleton-3d+physical-bones-remove-collision-exception :class 'skeleton-3d
  :bind "physical_bones_remove_collision_exception" :hash 2722037293)
 :void (exception rid))