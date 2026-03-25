(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-parameter-ref+set-parameter-name :class
  'visual-shader-node-parameter-ref :bind "set_parameter_name" :hash 83702148)
 :void (name string))

(defgmethod
 (visual-shader-node-parameter-ref+get-parameter-name :class
  'visual-shader-node-parameter-ref :bind "get_parameter_name" :hash 201670096)
 string)