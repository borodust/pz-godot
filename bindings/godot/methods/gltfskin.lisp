(common-lisp:in-package :%godot)


(defgmethod
 (gltfskin+get-skin-root :class 'gltfskin :bind "get_skin_root" :hash
  2455072627)
 int)

(defgmethod
 (gltfskin+set-skin-root :class 'gltfskin :bind "set_skin_root" :hash
  1286410249)
 :void (skin-root int))

(defgmethod
 (gltfskin+get-joints-original :class 'gltfskin :bind "get_joints_original"
  :hash 969006518)
 packed-int-32array)

(defgmethod
 (gltfskin+set-joints-original :class 'gltfskin :bind "set_joints_original"
  :hash 3614634198)
 :void (joints-original packed-int-32array))

(defgmethod
 (gltfskin+get-inverse-binds :class 'gltfskin :bind "get_inverse_binds" :hash
  2915620761)
 array)

(defgmethod
 (gltfskin+set-inverse-binds :class 'gltfskin :bind "set_inverse_binds" :hash
  381264803)
 :void (inverse-binds array))

(defgmethod
 (gltfskin+get-joints :class 'gltfskin :bind "get_joints" :hash 969006518)
 packed-int-32array)

(defgmethod
 (gltfskin+set-joints :class 'gltfskin :bind "set_joints" :hash 3614634198)
 :void (joints packed-int-32array))

(defgmethod
 (gltfskin+get-non-joints :class 'gltfskin :bind "get_non_joints" :hash
  969006518)
 packed-int-32array)

(defgmethod
 (gltfskin+set-non-joints :class 'gltfskin :bind "set_non_joints" :hash
  3614634198)
 :void (non-joints packed-int-32array))

(defgmethod
 (gltfskin+get-roots :class 'gltfskin :bind "get_roots" :hash 969006518)
 packed-int-32array)

(defgmethod
 (gltfskin+set-roots :class 'gltfskin :bind "set_roots" :hash 3614634198) :void
 (roots packed-int-32array))

(defgmethod
 (gltfskin+get-skeleton :class 'gltfskin :bind "get_skeleton" :hash 2455072627)
 int)

(defgmethod
 (gltfskin+set-skeleton :class 'gltfskin :bind "set_skeleton" :hash 1286410249)
 :void (skeleton int))

(defgmethod
 (gltfskin+get-joint-i-to-bone-i :class 'gltfskin :bind "get_joint_i_to_bone_i"
  :hash 2382534195)
 dictionary)

(defgmethod
 (gltfskin+set-joint-i-to-bone-i :class 'gltfskin :bind "set_joint_i_to_bone_i"
  :hash 4155329257)
 :void (joint-i-to-bone-i dictionary))

(defgmethod
 (gltfskin+get-joint-i-to-name :class 'gltfskin :bind "get_joint_i_to_name"
  :hash 2382534195)
 dictionary)

(defgmethod
 (gltfskin+set-joint-i-to-name :class 'gltfskin :bind "set_joint_i_to_name"
  :hash 4155329257)
 :void (joint-i-to-name dictionary))

(defgmethod
 (gltfskin+get-godot-skin :class 'gltfskin :bind "get_godot_skin" :hash
  1032037385)
 skin)

(defgmethod
 (gltfskin+set-godot-skin :class 'gltfskin :bind "set_godot_skin" :hash
  3971435618)
 :void (godot-skin skin))