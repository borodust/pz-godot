(common-lisp:in-package :%godot)


(defgmethod
 (bone-twist-disperser-3d+set-setting-count :class 'bone-twist-disperser-3d
  :bind "set_setting_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (bone-twist-disperser-3d+get-setting-count :class 'bone-twist-disperser-3d
  :bind "get_setting_count" :hash 3905245786)
 int)

(defgmethod
 (bone-twist-disperser-3d+clear-settings :class 'bone-twist-disperser-3d :bind
  "clear_settings" :hash 3218959716)
 :void)

(defgmethod
 (bone-twist-disperser-3d+set-mutable-bone-axes :class 'bone-twist-disperser-3d
  :bind "set_mutable_bone_axes" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (bone-twist-disperser-3d+are-bone-axes-mutable :class 'bone-twist-disperser-3d
  :bind "are_bone_axes_mutable" :hash 36873697)
 bool)

(defgmethod
 (bone-twist-disperser-3d+set-root-bone-name :class 'bone-twist-disperser-3d
  :bind "set_root_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (bone-twist-disperser-3d+get-root-bone-name :class 'bone-twist-disperser-3d
  :bind "get_root_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (bone-twist-disperser-3d+set-root-bone :class 'bone-twist-disperser-3d :bind
  "set_root_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (bone-twist-disperser-3d+get-root-bone :class 'bone-twist-disperser-3d :bind
  "get_root_bone" :hash 923996154)
 int (index int))

(defgmethod
 (bone-twist-disperser-3d+set-end-bone-name :class 'bone-twist-disperser-3d
  :bind "set_end_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (bone-twist-disperser-3d+get-end-bone-name :class 'bone-twist-disperser-3d
  :bind "get_end_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (bone-twist-disperser-3d+set-end-bone :class 'bone-twist-disperser-3d :bind
  "set_end_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (bone-twist-disperser-3d+get-end-bone :class 'bone-twist-disperser-3d :bind
  "get_end_bone" :hash 923996154)
 int (index int))

(defgmethod
 (bone-twist-disperser-3d+get-reference-bone-name :class
  'bone-twist-disperser-3d :bind "get_reference_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (bone-twist-disperser-3d+get-reference-bone :class 'bone-twist-disperser-3d
  :bind "get_reference_bone" :hash 923996154)
 int (index int))

(defgmethod
 (bone-twist-disperser-3d+set-extend-end-bone :class 'bone-twist-disperser-3d
  :bind "set_extend_end_bone" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (bone-twist-disperser-3d+is-end-bone-extended :class 'bone-twist-disperser-3d
  :bind "is_end_bone_extended" :hash 1116898809)
 bool (index int))

(defgmethod
 (bone-twist-disperser-3d+set-end-bone-direction :class
  'bone-twist-disperser-3d :bind "set_end_bone_direction" :hash 2838484201)
 :void (index int) (bone-direction skeleton-modifier-3d+bone-direction))

(defgmethod
 (bone-twist-disperser-3d+get-end-bone-direction :class
  'bone-twist-disperser-3d :bind "get_end_bone_direction" :hash 1843036459)
 skeleton-modifier-3d+bone-direction (index int))

(defgmethod
 (bone-twist-disperser-3d+set-twist-from-rest :class 'bone-twist-disperser-3d
  :bind "set_twist_from_rest" :hash 300928843)
 :void (index int) (enabled bool))

(defgmethod
 (bone-twist-disperser-3d+is-twist-from-rest :class 'bone-twist-disperser-3d
  :bind "is_twist_from_rest" :hash 1116898809)
 bool (index int))

(defgmethod
 (bone-twist-disperser-3d+set-twist-from :class 'bone-twist-disperser-3d :bind
  "set_twist_from" :hash 2823819782)
 :void (index int) (from quaternion))

(defgmethod
 (bone-twist-disperser-3d+get-twist-from :class 'bone-twist-disperser-3d :bind
  "get_twist_from" :hash 476865136)
 quaternion (index int))

(defgmethod
 (bone-twist-disperser-3d+set-disperse-mode :class 'bone-twist-disperser-3d
  :bind "set_disperse_mode" :hash 2954194337)
 :void (index int) (disperse-mode bone-twist-disperser-3d+disperse-mode))

(defgmethod
 (bone-twist-disperser-3d+get-disperse-mode :class 'bone-twist-disperser-3d
  :bind "get_disperse_mode" :hash 1326397005)
 bone-twist-disperser-3d+disperse-mode (index int))

(defgmethod
 (bone-twist-disperser-3d+set-weight-position :class 'bone-twist-disperser-3d
  :bind "set_weight_position" :hash 1602489585)
 :void (index int) (weight-position float))

(defgmethod
 (bone-twist-disperser-3d+get-weight-position :class 'bone-twist-disperser-3d
  :bind "get_weight_position" :hash 2339986948)
 float (index int))

(defgmethod
 (bone-twist-disperser-3d+set-damping-curve :class 'bone-twist-disperser-3d
  :bind "set_damping_curve" :hash 1447180063)
 :void (index int) (curve curve))

(defgmethod
 (bone-twist-disperser-3d+get-damping-curve :class 'bone-twist-disperser-3d
  :bind "get_damping_curve" :hash 747537754)
 curve (index int))

(defgmethod
 (bone-twist-disperser-3d+get-joint-bone-name :class 'bone-twist-disperser-3d
  :bind "get_joint_bone_name" :hash 1391810591)
 string (index int) (joint int))

(defgmethod
 (bone-twist-disperser-3d+get-joint-bone :class 'bone-twist-disperser-3d :bind
  "get_joint_bone" :hash 3175239445)
 int (index int) (joint int))

(defgmethod
 (bone-twist-disperser-3d+get-joint-twist-amount :class
  'bone-twist-disperser-3d :bind "get_joint_twist_amount" :hash 3085491603)
 float (index int) (joint int))

(defgmethod
 (bone-twist-disperser-3d+set-joint-twist-amount :class
  'bone-twist-disperser-3d :bind "set_joint_twist_amount" :hash 3506521499)
 :void (index int) (joint int) (twist-amount float))

(defgmethod
 (bone-twist-disperser-3d+get-joint-count :class 'bone-twist-disperser-3d :bind
  "get_joint_count" :hash 923996154)
 int (index int))