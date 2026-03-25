(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-transform-op+set-operator :class
  'visual-shader-node-transform-op :bind "set_operator" :hash 2287310733)
 :void (op visual-shader-node-transform-op+operator))

(defgmethod
 (visual-shader-node-transform-op+get-operator :class
  'visual-shader-node-transform-op :bind "get_operator" :hash 1238663601)
 visual-shader-node-transform-op+operator)