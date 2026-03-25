(common-lisp:in-package :%godot)


(defgmethod
 (bone-constraint-3d+set-amount :class 'bone-constraint-3d :bind "set_amount"
  :hash 1602489585)
 :void (index int) (amount float))

(defgmethod
 (bone-constraint-3d+get-amount :class 'bone-constraint-3d :bind "get_amount"
  :hash 2339986948)
 float (index int))

(defgmethod
 (bone-constraint-3d+set-apply-bone-name :class 'bone-constraint-3d :bind
  "set_apply_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (bone-constraint-3d+get-apply-bone-name :class 'bone-constraint-3d :bind
  "get_apply_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (bone-constraint-3d+set-apply-bone :class 'bone-constraint-3d :bind
  "set_apply_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (bone-constraint-3d+get-apply-bone :class 'bone-constraint-3d :bind
  "get_apply_bone" :hash 923996154)
 int (index int))

(defgmethod
 (bone-constraint-3d+set-reference-type :class 'bone-constraint-3d :bind
  "set_reference_type" :hash 1830520418)
 :void (index int) (type bone-constraint-3d+reference-type))

(defgmethod
 (bone-constraint-3d+get-reference-type :class 'bone-constraint-3d :bind
  "get_reference_type" :hash 3456416152)
 bone-constraint-3d+reference-type (index int))

(defgmethod
 (bone-constraint-3d+set-reference-bone-name :class 'bone-constraint-3d :bind
  "set_reference_bone_name" :hash 501894301)
 :void (index int) (bone-name string))

(defgmethod
 (bone-constraint-3d+get-reference-bone-name :class 'bone-constraint-3d :bind
  "get_reference_bone_name" :hash 844755477)
 string (index int))

(defgmethod
 (bone-constraint-3d+set-reference-bone :class 'bone-constraint-3d :bind
  "set_reference_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (bone-constraint-3d+get-reference-bone :class 'bone-constraint-3d :bind
  "get_reference_bone" :hash 923996154)
 int (index int))

(defgmethod
 (bone-constraint-3d+set-reference-node :class 'bone-constraint-3d :bind
  "set_reference_node" :hash 2761262315)
 :void (index int) (node node-path))

(defgmethod
 (bone-constraint-3d+get-reference-node :class 'bone-constraint-3d :bind
  "get_reference_node" :hash 408788394)
 node-path (index int))

(defgmethod
 (bone-constraint-3d+set-setting-count :class 'bone-constraint-3d :bind
  "set_setting_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (bone-constraint-3d+get-setting-count :class 'bone-constraint-3d :bind
  "get_setting_count" :hash 3905245786)
 int)

(defgmethod
 (bone-constraint-3d+clear-setting :class 'bone-constraint-3d :bind
  "clear_setting" :hash 3218959716)
 :void)