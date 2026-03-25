(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-comment+set-description :class 'visual-shader-node-comment
  :bind "set_description" :hash 83702148)
 :void (description string))

(defgmethod
 (visual-shader-node-comment+get-description :class 'visual-shader-node-comment
  :bind "get_description" :hash 201670096)
 string)