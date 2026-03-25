(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-int-op+set-operator :class 'visual-shader-node-int-op
  :bind "set_operator" :hash 1677909323)
 :void (op visual-shader-node-int-op+operator))

(defgmethod
 (visual-shader-node-int-op+get-operator :class 'visual-shader-node-int-op
  :bind "get_operator" :hash 1236987913)
 visual-shader-node-int-op+operator)