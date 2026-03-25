(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-expression+set-expression :class
  'visual-shader-node-expression :bind "set_expression" :hash 83702148)
 :void (expression string))

(defgmethod
 (visual-shader-node-expression+get-expression :class
  'visual-shader-node-expression :bind "get_expression" :hash 201670096)
 string)