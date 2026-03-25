(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-uint-op+set-operator :class 'visual-shader-node-uint-op
  :bind "set_operator" :hash 3463048345)
 :void (op visual-shader-node-uint-op+operator))

(defgmethod
 (visual-shader-node-uint-op+get-operator :class 'visual-shader-node-uint-op
  :bind "get_operator" :hash 256631461)
 visual-shader-node-uint-op+operator)