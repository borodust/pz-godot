(common-lisp:in-package :%godot)


(defgmethod
 (bone-map+get-profile :class 'bone-map :bind "get_profile" :hash 4291782652)
 skeleton-profile)

(defgmethod
 (bone-map+set-profile :class 'bone-map :bind "set_profile" :hash 3870374136)
 :void (profile skeleton-profile))

(defgmethod
 (bone-map+get-skeleton-bone-name :class 'bone-map :bind
  "get_skeleton_bone_name" :hash 1965194235)
 string-name (profile-bone-name string-name))

(defgmethod
 (bone-map+set-skeleton-bone-name :class 'bone-map :bind
  "set_skeleton_bone_name" :hash 3740211285)
 :void (profile-bone-name string-name) (skeleton-bone-name string-name))

(defgmethod
 (bone-map+find-profile-bone-name :class 'bone-map :bind
  "find_profile_bone_name" :hash 1965194235)
 string-name (skeleton-bone-name string-name))