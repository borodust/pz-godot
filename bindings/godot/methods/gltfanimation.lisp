(common-lisp:in-package :%godot)


(defgmethod
 (gltfanimation+get-original-name :class 'gltfanimation :bind
  "get_original_name" :hash 2841200299)
 string)

(defgmethod
 (gltfanimation+set-original-name :class 'gltfanimation :bind
  "set_original_name" :hash 83702148)
 :void (original-name string))

(defgmethod
 (gltfanimation+get-loop :class 'gltfanimation :bind "get_loop" :hash 36873697)
 bool)

(defgmethod
 (gltfanimation+set-loop :class 'gltfanimation :bind "set_loop" :hash
  2586408642)
 :void (loop bool))

(defgmethod
 (gltfanimation+get-additional-data :class 'gltfanimation :bind
  "get_additional_data" :hash 2138907829)
 variant (extension-name string-name))

(defgmethod
 (gltfanimation+set-additional-data :class 'gltfanimation :bind
  "set_additional_data" :hash 3776071444)
 :void (extension-name string-name) (additional-data variant))