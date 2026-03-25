(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-input+set-input-name :class 'visual-shader-node-input
  :bind "set_input_name" :hash 83702148)
 :void (name string))

(defgmethod
 (visual-shader-node-input+get-input-name :class 'visual-shader-node-input
  :bind "get_input_name" :hash 201670096)
 string)

(defgmethod
 (visual-shader-node-input+get-input-real-name :class 'visual-shader-node-input
  :bind "get_input_real_name" :hash 201670096)
 string)