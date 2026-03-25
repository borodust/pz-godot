(common-lisp:in-package :%godot)


(defgmethod
 (gltfskeleton+get-joints :class 'gltfskeleton :bind "get_joints" :hash
  969006518)
 packed-int-32array)

(defgmethod
 (gltfskeleton+set-joints :class 'gltfskeleton :bind "set_joints" :hash
  3614634198)
 :void (joints packed-int-32array))

(defgmethod
 (gltfskeleton+get-roots :class 'gltfskeleton :bind "get_roots" :hash
  969006518)
 packed-int-32array)

(defgmethod
 (gltfskeleton+set-roots :class 'gltfskeleton :bind "set_roots" :hash
  3614634198)
 :void (roots packed-int-32array))

(defgmethod
 (gltfskeleton+get-godot-skeleton :class 'gltfskeleton :bind
  "get_godot_skeleton" :hash 1814733083)
 skeleton-3d)

(defgmethod
 (gltfskeleton+get-unique-names :class 'gltfskeleton :bind "get_unique_names"
  :hash 2915620761)
 array)

(defgmethod
 (gltfskeleton+set-unique-names :class 'gltfskeleton :bind "set_unique_names"
  :hash 381264803)
 :void (unique-names array))

(defgmethod
 (gltfskeleton+get-godot-bone-node :class 'gltfskeleton :bind
  "get_godot_bone_node" :hash 2382534195)
 dictionary)

(defgmethod
 (gltfskeleton+set-godot-bone-node :class 'gltfskeleton :bind
  "set_godot_bone_node" :hash 4155329257)
 :void (godot-bone-node dictionary))

(defgmethod
 (gltfskeleton+get-bone-attachment-count :class 'gltfskeleton :bind
  "get_bone_attachment_count" :hash 2455072627)
 int)

(defgmethod
 (gltfskeleton+get-bone-attachment :class 'gltfskeleton :bind
  "get_bone_attachment" :hash 945440495)
 bone-attachment-3d (idx int))