(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-color-op+set-operator :class 'visual-shader-node-color-op
  :bind "set_operator" :hash 4260370673)
 :void (op visual-shader-node-color-op+operator))

(defgmethod
 (visual-shader-node-color-op+get-operator :class 'visual-shader-node-color-op
  :bind "get_operator" :hash 1950956529)
 visual-shader-node-color-op+operator)