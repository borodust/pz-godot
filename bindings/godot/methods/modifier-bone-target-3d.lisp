(common-lisp:in-package :%godot)


(defgmethod
 (modifier-bone-target-3d+set-bone-name :class 'modifier-bone-target-3d :bind
  "set_bone_name" :hash 83702148)
 :void (bone-name string))

(defgmethod
 (modifier-bone-target-3d+get-bone-name :class 'modifier-bone-target-3d :bind
  "get_bone_name" :hash 201670096)
 string)

(defgmethod
 (modifier-bone-target-3d+set-bone :class 'modifier-bone-target-3d :bind
  "set_bone" :hash 1286410249)
 :void (bone int))

(defgmethod
 (modifier-bone-target-3d+get-bone :class 'modifier-bone-target-3d :bind
  "get_bone" :hash 3905245786)
 int)