(common-lisp:in-package :%godot)


(defgmethod
 (limit-angular-velocity-modifier-3d+set-root-bone-name :class
  'limit-angular-velocity-modifier-3d :bind "set_root_bone_name" :hash
  501894301)
 :void (index int) (bone-name string))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-root-bone-name :class
  'limit-angular-velocity-modifier-3d :bind "get_root_bone_name" :hash
  844755477)
 string (index int))

(defgmethod
 (limit-angular-velocity-modifier-3d+set-root-bone :class
  'limit-angular-velocity-modifier-3d :bind "set_root_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-root-bone :class
  'limit-angular-velocity-modifier-3d :bind "get_root_bone" :hash 923996154)
 int (index int))

(defgmethod
 (limit-angular-velocity-modifier-3d+set-end-bone-name :class
  'limit-angular-velocity-modifier-3d :bind "set_end_bone_name" :hash
  501894301)
 :void (index int) (bone-name string))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-end-bone-name :class
  'limit-angular-velocity-modifier-3d :bind "get_end_bone_name" :hash
  844755477)
 string (index int))

(defgmethod
 (limit-angular-velocity-modifier-3d+set-end-bone :class
  'limit-angular-velocity-modifier-3d :bind "set_end_bone" :hash 3937882851)
 :void (index int) (bone int))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-end-bone :class
  'limit-angular-velocity-modifier-3d :bind "get_end_bone" :hash 923996154)
 int (index int))

(defgmethod
 (limit-angular-velocity-modifier-3d+set-chain-count :class
  'limit-angular-velocity-modifier-3d :bind "set_chain_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-chain-count :class
  'limit-angular-velocity-modifier-3d :bind "get_chain_count" :hash 3905245786)
 int)

(defgmethod
 (limit-angular-velocity-modifier-3d+clear-chains :class
  'limit-angular-velocity-modifier-3d :bind "clear_chains" :hash 3218959716)
 :void)

(defgmethod
 (limit-angular-velocity-modifier-3d+set-max-angular-velocity :class
  'limit-angular-velocity-modifier-3d :bind "set_max_angular_velocity" :hash
  373806689)
 :void (angular-velocity float))

(defgmethod
 (limit-angular-velocity-modifier-3d+get-max-angular-velocity :class
  'limit-angular-velocity-modifier-3d :bind "get_max_angular_velocity" :hash
  1740695150)
 float)

(defgmethod
 (limit-angular-velocity-modifier-3d+set-exclude :class
  'limit-angular-velocity-modifier-3d :bind "set_exclude" :hash 2586408642)
 :void (exclude bool))

(defgmethod
 (limit-angular-velocity-modifier-3d+is-exclude :class
  'limit-angular-velocity-modifier-3d :bind "is_exclude" :hash 36873697)
 bool)

(defgmethod
 (limit-angular-velocity-modifier-3d+reset :class
  'limit-angular-velocity-modifier-3d :bind "reset" :hash 3218959716)
 :void)