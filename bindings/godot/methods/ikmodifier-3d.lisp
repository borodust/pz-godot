(common-lisp:in-package :%godot)


(defgmethod
 (ikmodifier-3d+set-setting-count :class 'ikmodifier-3d :bind
  "set_setting_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (ikmodifier-3d+get-setting-count :class 'ikmodifier-3d :bind
  "get_setting_count" :hash 3905245786)
 int)

(defgmethod
 (ikmodifier-3d+clear-settings :class 'ikmodifier-3d :bind "clear_settings"
  :hash 3218959716)
 :void)

(defgmethod
 (ikmodifier-3d+set-mutable-bone-axes :class 'ikmodifier-3d :bind
  "set_mutable_bone_axes" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (ikmodifier-3d+are-bone-axes-mutable :class 'ikmodifier-3d :bind
  "are_bone_axes_mutable" :hash 36873697)
 bool)

(defgmethod
 (ikmodifier-3d+reset :class 'ikmodifier-3d :bind "reset" :hash 3218959716)
 :void)