(common-lisp:in-package :%godot)


(defgmethod
 (spring-bone-collision-3d+get-skeleton :class 'spring-bone-collision-3d :bind
  "get_skeleton" :hash 1488626673)
 skeleton-3d)

(defgmethod
 (spring-bone-collision-3d+set-bone-name :class 'spring-bone-collision-3d :bind
  "set_bone_name" :hash 83702148)
 :void (bone-name string))

(defgmethod
 (spring-bone-collision-3d+get-bone-name :class 'spring-bone-collision-3d :bind
  "get_bone_name" :hash 201670096)
 string)

(defgmethod
 (spring-bone-collision-3d+set-bone :class 'spring-bone-collision-3d :bind
  "set_bone" :hash 1286410249)
 :void (bone int))

(defgmethod
 (spring-bone-collision-3d+get-bone :class 'spring-bone-collision-3d :bind
  "get_bone" :hash 3905245786)
 int)

(defgmethod
 (spring-bone-collision-3d+set-position-offset :class 'spring-bone-collision-3d
  :bind "set_position_offset" :hash 3460891852)
 :void (offset vector-3))

(defgmethod
 (spring-bone-collision-3d+get-position-offset :class 'spring-bone-collision-3d
  :bind "get_position_offset" :hash 3360562783)
 vector-3)

(defgmethod
 (spring-bone-collision-3d+set-rotation-offset :class 'spring-bone-collision-3d
  :bind "set_rotation_offset" :hash 1727505552)
 :void (offset quaternion))

(defgmethod
 (spring-bone-collision-3d+get-rotation-offset :class 'spring-bone-collision-3d
  :bind "get_rotation_offset" :hash 1222331677)
 quaternion)