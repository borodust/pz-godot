(common-lisp:in-package :%godot)


(defgmethod
 (bone-2d+set-rest :class 'bone-2d :bind "set_rest" :hash 2761652528) :void
 (rest transform-2d))

(defgmethod
 (bone-2d+get-rest :class 'bone-2d :bind "get_rest" :hash 3814499831)
 transform-2d)

(defgmethod
 (bone-2d+apply-rest :class 'bone-2d :bind "apply_rest" :hash 3218959716) :void)

(defgmethod
 (bone-2d+get-skeleton-rest :class 'bone-2d :bind "get_skeleton_rest" :hash
  3814499831)
 transform-2d)

(defgmethod
 (bone-2d+get-index-in-skeleton :class 'bone-2d :bind "get_index_in_skeleton"
  :hash 3905245786)
 int)

(defgmethod
 (bone-2d+set-autocalculate-length-and-angle :class 'bone-2d :bind
  "set_autocalculate_length_and_angle" :hash 2586408642)
 :void (auto-calculate bool))

(defgmethod
 (bone-2d+get-autocalculate-length-and-angle :class 'bone-2d :bind
  "get_autocalculate_length_and_angle" :hash 36873697)
 bool)

(defgmethod
 (bone-2d+set-length :class 'bone-2d :bind "set_length" :hash 373806689) :void
 (length float))

(defgmethod
 (bone-2d+get-length :class 'bone-2d :bind "get_length" :hash 1740695150) float)

(defgmethod
 (bone-2d+set-bone-angle :class 'bone-2d :bind "set_bone_angle" :hash
  373806689)
 :void (angle float))

(defgmethod
 (bone-2d+get-bone-angle :class 'bone-2d :bind "get_bone_angle" :hash
  1740695150)
 float)