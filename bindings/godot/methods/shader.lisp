(common-lisp:in-package :%godot)


(defgmethod (shader+get-mode :class 'shader :bind "get_mode" :hash 3392948163)
 shader+mode)

(defgmethod (shader+set-code :class 'shader :bind "set_code" :hash 83702148)
 :void (code string))

(defgmethod (shader+get-code :class 'shader :bind "get_code" :hash 201670096)
 string)

(defgmethod
 (shader+set-default-texture-parameter :class 'shader :bind
  "set_default_texture_parameter" :hash 3850209648)
 :void (name string-name) (texture texture) (index int))

(defgmethod
 (shader+get-default-texture-parameter :class 'shader :bind
  "get_default_texture_parameter" :hash 4213877425)
 texture (name string-name) (index int))

(defgmethod
 (shader+get-shader-uniform-list :class 'shader :bind "get_shader_uniform_list"
  :hash 1230511656)
 array (get-groups bool))

(defgmethod
 (shader+inspect-native-shader-code :class 'shader :bind
  "inspect_native_shader_code" :hash 3218959716)
 :void)