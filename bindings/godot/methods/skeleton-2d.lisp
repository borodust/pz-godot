(common-lisp:in-package :%godot)


(defgmethod
 (skeleton-2d+get-bone-count :class 'skeleton-2d :bind "get_bone_count" :hash
  3905245786)
 int)

(defgmethod
 (skeleton-2d+get-bone :class 'skeleton-2d :bind "get_bone" :hash 2556267111)
 bone-2d (idx int))

(defgmethod
 (skeleton-2d+get-skeleton :class 'skeleton-2d :bind "get_skeleton" :hash
  2944877500)
 rid)

(defgmethod
 (skeleton-2d+set-modification-stack :class 'skeleton-2d :bind
  "set_modification_stack" :hash 3907307132)
 :void (modification-stack skeleton-modification-stack-2d))

(defgmethod
 (skeleton-2d+get-modification-stack :class 'skeleton-2d :bind
  "get_modification_stack" :hash 2107508396)
 skeleton-modification-stack-2d)

(defgmethod
 (skeleton-2d+execute-modifications :class 'skeleton-2d :bind
  "execute_modifications" :hash 1005356550)
 :void (delta float) (execution-mode int))

(defgmethod
 (skeleton-2d+set-bone-local-pose-override :class 'skeleton-2d :bind
  "set_bone_local_pose_override" :hash 555457532)
 :void (bone-idx int) (override-pose transform-2d) (strength float)
 (persistent bool))

(defgmethod
 (skeleton-2d+get-bone-local-pose-override :class 'skeleton-2d :bind
  "get_bone_local_pose_override" :hash 2995540667)
 transform-2d (bone-idx int))