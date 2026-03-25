(common-lisp:in-package :%godot)


(defgmethod
 (spring-bone-simulator-3d+set-root-bone-name :class 'spring-bone-simulator-3d
  :bind "set_root_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (spring-bone-simulator-3d+get-root-bone-name :class 'spring-bone-simulator-3d
  :bind "get_root_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (spring-bone-simulator-3d+set-root-bone :class 'spring-bone-simulator-3d :bind
  "set_root_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (spring-bone-simulator-3d+get-root-bone :class 'spring-bone-simulator-3d :bind
  "get_root_bone" :hash 923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+set-end-bone-name :class 'spring-bone-simulator-3d
  :bind "set_end_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (spring-bone-simulator-3d+get-end-bone-name :class 'spring-bone-simulator-3d
  :bind "get_end_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (spring-bone-simulator-3d+set-end-bone :class 'spring-bone-simulator-3d :bind
  "set_end_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (spring-bone-simulator-3d+get-end-bone :class 'spring-bone-simulator-3d :bind
  "get_end_bone" :hash 923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+set-extend-end-bone :class 'spring-bone-simulator-3d
  :bind "set_extend_end_bone" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (spring-bone-simulator-3d+is-end-bone-extended :class
  'spring-bone-simulator-3d :bind "is_end_bone_extended" :hash 1116898809)
 bool (index int))

(defgmethod
 (spring-bone-simulator-3d+set-end-bone-direction :class
  'spring-bone-simulator-3d :bind "set_end_bone_direction" :hash 2838484201)
 :void (index int) (bone-direction skeleton-modifier-3d+bone-direction))

(defgmethod
 (spring-bone-simulator-3d+get-end-bone-direction :class
  'spring-bone-simulator-3d :bind "get_end_bone_direction" :hash 1843036459)
 skeleton-modifier-3d+bone-direction (index int))

(defgmethod
 (spring-bone-simulator-3d+set-end-bone-length :class 'spring-bone-simulator-3d
  :bind "set_end_bone_length" :hash 1602489585)
 :void (index int) (length float))

(defgmethod
 (spring-bone-simulator-3d+get-end-bone-length :class 'spring-bone-simulator-3d
  :bind "get_end_bone_length" :hash 2339986948)
 float (index int))

(defgmethod
 (spring-bone-simulator-3d+set-center-from :class 'spring-bone-simulator-3d
  :bind "set_center_from" :hash 2551505749)
 :void (index int) (center-from spring-bone-simulator-3d+center-from))

(defgmethod
 (spring-bone-simulator-3d+get-center-from :class 'spring-bone-simulator-3d
  :bind "get_center_from" :hash 2721930813)
 spring-bone-simulator-3d+center-from (index int))

(defgmethod
 (spring-bone-simulator-3d+set-center-node :class 'spring-bone-simulator-3d
  :bind "set_center_node" :hash 2761262315)
 :void (index int) (node-path node-path))

(defgmethod
 (spring-bone-simulator-3d+get-center-node :class 'spring-bone-simulator-3d
  :bind "get_center_node" :hash 408788394)
 node-path (index int))

(defgmethod
 (spring-bone-simulator-3d+set-center-bone-name :class
  'spring-bone-simulator-3d :bind "set_center_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (spring-bone-simulator-3d+get-center-bone-name :class
  'spring-bone-simulator-3d :bind "get_center_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (spring-bone-simulator-3d+set-center-bone :class 'spring-bone-simulator-3d
  :bind "set_center_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (spring-bone-simulator-3d+get-center-bone :class 'spring-bone-simulator-3d
  :bind "get_center_bone" :hash 923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+set-radius :class 'spring-bone-simulator-3d :bind
  "set_radius" :hash 1602489585)
 :void (index int) (radius float))

(defgmethod
 (spring-bone-simulator-3d+get-radius :class 'spring-bone-simulator-3d :bind
  "get_radius" :hash 2339986948)
 float (index int))

(defgmethod
 (spring-bone-simulator-3d+set-rotation-axis :class 'spring-bone-simulator-3d
  :bind "set_rotation_axis" :hash 1539703856)
 :void (index int) (axis skeleton-modifier-3d+rotation-axis))

(defgmethod
 (spring-bone-simulator-3d+get-rotation-axis :class 'spring-bone-simulator-3d
  :bind "get_rotation_axis" :hash 2844851118)
 skeleton-modifier-3d+rotation-axis (index int))

(defgmethod
 (spring-bone-simulator-3d+set-rotation-axis-vector :class
  'spring-bone-simulator-3d :bind "set_rotation_axis_vector" :hash 1530502735)
 :void (index int) (vector vector-3))

(defgmethod
 (spring-bone-simulator-3d+get-rotation-axis-vector :class
  'spring-bone-simulator-3d :bind "get_rotation_axis_vector" :hash 711720468)
 vector-3 (index int))

(defgmethod
 (spring-bone-simulator-3d+set-radius-damping-curve :class
  'spring-bone-simulator-3d :bind "set_radius_damping_curve" :hash 1447180063)
 :void (index int) (curve curve))

(defgmethod
 (spring-bone-simulator-3d+get-radius-damping-curve :class
  'spring-bone-simulator-3d :bind "get_radius_damping_curve" :hash 747537754)
 curve (index int))

(defgmethod
 (spring-bone-simulator-3d+set-stiffness :class 'spring-bone-simulator-3d :bind
  "set_stiffness" :hash 1602489585)
 :void (index int) (stiffness float))

(defgmethod
 (spring-bone-simulator-3d+get-stiffness :class 'spring-bone-simulator-3d :bind
  "get_stiffness" :hash 2339986948)
 float (index int))

(defgmethod
 (spring-bone-simulator-3d+set-stiffness-damping-curve :class
  'spring-bone-simulator-3d :bind "set_stiffness_damping_curve" :hash
  1447180063)
 :void (index int) (curve curve))

(defgmethod
 (spring-bone-simulator-3d+get-stiffness-damping-curve :class
  'spring-bone-simulator-3d :bind "get_stiffness_damping_curve" :hash
  747537754)
 curve (index int))

(defgmethod
 (spring-bone-simulator-3d+set-drag :class 'spring-bone-simulator-3d :bind
  "set_drag" :hash 1602489585)
 :void (index int) (drag float))

(defgmethod
 (spring-bone-simulator-3d+get-drag :class 'spring-bone-simulator-3d :bind
  "get_drag" :hash 2339986948)
 float (index int))

(defgmethod
 (spring-bone-simulator-3d+set-drag-damping-curve :class
  'spring-bone-simulator-3d :bind "set_drag_damping_curve" :hash 1447180063)
 :void (index int) (curve curve))

(defgmethod
 (spring-bone-simulator-3d+get-drag-damping-curve :class
  'spring-bone-simulator-3d :bind "get_drag_damping_curve" :hash 747537754)
 curve (index int))

(defgmethod
 (spring-bone-simulator-3d+set-gravity :class 'spring-bone-simulator-3d :bind
  "set_gravity" :hash 1602489585)
 :void (index int) (gravity float))

(defgmethod
 (spring-bone-simulator-3d+get-gravity :class 'spring-bone-simulator-3d :bind
  "get_gravity" :hash 2339986948)
 float (index int))

(defgmethod
 (spring-bone-simulator-3d+set-gravity-damping-curve :class
  'spring-bone-simulator-3d :bind "set_gravity_damping_curve" :hash 1447180063)
 :void (index int) (curve curve))

(defgmethod
 (spring-bone-simulator-3d+get-gravity-damping-curve :class
  'spring-bone-simulator-3d :bind "get_gravity_damping_curve" :hash 747537754)
 curve (index int))

(defgmethod
 (spring-bone-simulator-3d+set-gravity-direction :class
  'spring-bone-simulator-3d :bind "set_gravity_direction" :hash 1530502735)
 :void (index int) (gravity-direction vector-3))

(defgmethod
 (spring-bone-simulator-3d+get-gravity-direction :class
  'spring-bone-simulator-3d :bind "get_gravity_direction" :hash 711720468)
 vector-3 (index int))

(defgmethod
 (spring-bone-simulator-3d+set-setting-count :class 'spring-bone-simulator-3d
  :bind "set_setting_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (spring-bone-simulator-3d+get-setting-count :class 'spring-bone-simulator-3d
  :bind "get_setting_count" :hash 3905245786)
 int)

(defgmethod
 (spring-bone-simulator-3d+clear-settings :class 'spring-bone-simulator-3d
  :bind "clear_settings" :hash 3218959716)
 :void)

(defgmethod
 (spring-bone-simulator-3d+set-individual-config :class
  'spring-bone-simulator-3d :bind "set_individual_config" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (spring-bone-simulator-3d+is-config-individual :class
  'spring-bone-simulator-3d :bind "is_config_individual" :hash 1116898809)
 bool (index int))

(defgmethod
 (spring-bone-simulator-3d+get-joint-bone-name :class 'spring-bone-simulator-3d
  :bind "get_joint_bone_name" :hash 1391810591)
 string (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+get-joint-bone :class 'spring-bone-simulator-3d
  :bind "get_joint_bone" :hash 3175239445)
 int (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-rotation-axis :class
  'spring-bone-simulator-3d :bind "set_joint_rotation_axis" :hash 1391134969)
 :void (index int) (joint int) (axis skeleton-modifier-3d+rotation-axis))

(defgmethod
 (spring-bone-simulator-3d+get-joint-rotation-axis :class
  'spring-bone-simulator-3d :bind "get_joint_rotation_axis" :hash 3312594080)
 skeleton-modifier-3d+rotation-axis (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-rotation-axis-vector :class
  'spring-bone-simulator-3d :bind "set_joint_rotation_axis_vector" :hash
  2866752138)
 :void (index int) (joint int) (vector vector-3))

(defgmethod
 (spring-bone-simulator-3d+get-joint-rotation-axis-vector :class
  'spring-bone-simulator-3d :bind "get_joint_rotation_axis_vector" :hash
  1592972041)
 vector-3 (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-radius :class 'spring-bone-simulator-3d
  :bind "set_joint_radius" :hash 3506521499)
 :void (index int) (joint int) (radius float))

(defgmethod
 (spring-bone-simulator-3d+get-joint-radius :class 'spring-bone-simulator-3d
  :bind "get_joint_radius" :hash 3085491603)
 float (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-stiffness :class 'spring-bone-simulator-3d
  :bind "set_joint_stiffness" :hash 3506521499)
 :void (index int) (joint int) (stiffness float))

(defgmethod
 (spring-bone-simulator-3d+get-joint-stiffness :class 'spring-bone-simulator-3d
  :bind "get_joint_stiffness" :hash 3085491603)
 float (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-drag :class 'spring-bone-simulator-3d
  :bind "set_joint_drag" :hash 3506521499)
 :void (index int) (joint int) (drag float))

(defgmethod
 (spring-bone-simulator-3d+get-joint-drag :class 'spring-bone-simulator-3d
  :bind "get_joint_drag" :hash 3085491603)
 float (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-gravity :class 'spring-bone-simulator-3d
  :bind "set_joint_gravity" :hash 3506521499)
 :void (index int) (joint int) (gravity float))

(defgmethod
 (spring-bone-simulator-3d+get-joint-gravity :class 'spring-bone-simulator-3d
  :bind "get_joint_gravity" :hash 3085491603)
 float (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+set-joint-gravity-direction :class
  'spring-bone-simulator-3d :bind "set_joint_gravity_direction" :hash
  2866752138)
 :void (index int) (joint int) (gravity-direction vector-3))

(defgmethod
 (spring-bone-simulator-3d+get-joint-gravity-direction :class
  'spring-bone-simulator-3d :bind "get_joint_gravity_direction" :hash
  1592972041)
 vector-3 (index int) (joint int))

(defgmethod
 (spring-bone-simulator-3d+get-joint-count :class 'spring-bone-simulator-3d
  :bind "get_joint_count" :hash 923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+set-enable-all-child-collisions :class
  'spring-bone-simulator-3d :bind "set_enable_all_child_collisions" :hash
  300928843)
 :void (index int) (enabled bool))

(defgmethod
 (spring-bone-simulator-3d+are-all-child-collisions-enabled :class
  'spring-bone-simulator-3d :bind "are_all_child_collisions_enabled" :hash
  1116898809)
 bool (index int))

(defgmethod
 (spring-bone-simulator-3d+set-exclude-collision-path :class
  'spring-bone-simulator-3d :bind "set_exclude_collision_path" :hash 132481804)
 :void (index int) (collision int) (node-path node-path))

(defgmethod
 (spring-bone-simulator-3d+get-exclude-collision-path :class
  'spring-bone-simulator-3d :bind "get_exclude_collision_path" :hash 464924783)
 node-path (index int) (collision int))

(defgmethod
 (spring-bone-simulator-3d+set-exclude-collision-count :class
  'spring-bone-simulator-3d :bind "set_exclude_collision_count" :hash
  3937882851)
 :void (index int) (count int))

(defgmethod
 (spring-bone-simulator-3d+get-exclude-collision-count :class
  'spring-bone-simulator-3d :bind "get_exclude_collision_count" :hash
  923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+clear-exclude-collisions :class
  'spring-bone-simulator-3d :bind "clear_exclude_collisions" :hash 1286410249)
 :void (index int))

(defgmethod
 (spring-bone-simulator-3d+set-collision-path :class 'spring-bone-simulator-3d
  :bind "set_collision_path" :hash 132481804)
 :void (index int) (collision int) (node-path node-path))

(defgmethod
 (spring-bone-simulator-3d+get-collision-path :class 'spring-bone-simulator-3d
  :bind "get_collision_path" :hash 464924783)
 node-path (index int) (collision int))

(defgmethod
 (spring-bone-simulator-3d+set-collision-count :class 'spring-bone-simulator-3d
  :bind "set_collision_count" :hash 3937882851)
 :void (index int) (count int))

(defgmethod
 (spring-bone-simulator-3d+get-collision-count :class 'spring-bone-simulator-3d
  :bind "get_collision_count" :hash 923996154)
 int (index int))

(defgmethod
 (spring-bone-simulator-3d+clear-collisions :class 'spring-bone-simulator-3d
  :bind "clear_collisions" :hash 1286410249)
 :void (index int))

(defgmethod
 (spring-bone-simulator-3d+set-external-force :class 'spring-bone-simulator-3d
  :bind "set_external_force" :hash 3460891852)
 :void (force vector-3))

(defgmethod
 (spring-bone-simulator-3d+get-external-force :class 'spring-bone-simulator-3d
  :bind "get_external_force" :hash 3360562783)
 vector-3)

(defgmethod
 (spring-bone-simulator-3d+set-mutable-bone-axes :class
  'spring-bone-simulator-3d :bind "set_mutable_bone_axes" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (spring-bone-simulator-3d+are-bone-axes-mutable :class
  'spring-bone-simulator-3d :bind "are_bone_axes_mutable" :hash 36873697)
 bool)

(defgmethod
 (spring-bone-simulator-3d+reset :class 'spring-bone-simulator-3d :bind "reset"
  :hash 3218959716)
 :void)