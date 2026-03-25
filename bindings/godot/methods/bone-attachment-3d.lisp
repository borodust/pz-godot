(common-lisp:in-package :%godot)


(defgmethod
 (bone-attachment-3d+get-skeleton :class 'bone-attachment-3d :bind
  "get_skeleton" :hash 1814733083)
 skeleton-3d)

(defgmethod
 (bone-attachment-3d+set-bone-name :class 'bone-attachment-3d :bind
  "set_bone_name" :hash 83702148)
 :void (bone-name string))

(defgmethod
 (bone-attachment-3d+get-bone-name :class 'bone-attachment-3d :bind
  "get_bone_name" :hash 201670096)
 string)

(defgmethod
 (bone-attachment-3d+set-bone-idx :class 'bone-attachment-3d :bind
  "set_bone_idx" :hash 1286410249)
 :void (bone-idx int))

(defgmethod
 (bone-attachment-3d+get-bone-idx :class 'bone-attachment-3d :bind
  "get_bone_idx" :hash 3905245786)
 int)

(defgmethod
 (bone-attachment-3d+on-skeleton-update :class 'bone-attachment-3d :bind
  "on_skeleton_update" :hash 3218959716)
 :void)

(defgmethod
 (bone-attachment-3d+set-override-pose :class 'bone-attachment-3d :bind
  "set_override_pose" :hash 2586408642)
 :void (override-pose bool))

(defgmethod
 (bone-attachment-3d+get-override-pose :class 'bone-attachment-3d :bind
  "get_override_pose" :hash 36873697)
 bool)

(defgmethod
 (bone-attachment-3d+set-use-external-skeleton :class 'bone-attachment-3d :bind
  "set_use_external_skeleton" :hash 2586408642)
 :void (use-external-skeleton bool))

(defgmethod
 (bone-attachment-3d+get-use-external-skeleton :class 'bone-attachment-3d :bind
  "get_use_external_skeleton" :hash 36873697)
 bool)

(defgmethod
 (bone-attachment-3d+set-external-skeleton :class 'bone-attachment-3d :bind
  "set_external_skeleton" :hash 1348162250)
 :void (external-skeleton node-path))

(defgmethod
 (bone-attachment-3d+get-external-skeleton :class 'bone-attachment-3d :bind
  "get_external_skeleton" :hash 4075236667)
 node-path)