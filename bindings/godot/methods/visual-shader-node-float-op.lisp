(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-float-op+set-operator :class 'visual-shader-node-float-op
  :bind "set_operator" :hash 2488468047)
 :void (op visual-shader-node-float-op+operator))

(defgmethod
 (visual-shader-node-float-op+get-operator :class 'visual-shader-node-float-op
  :bind "get_operator" :hash 1867979390)
 visual-shader-node-float-op+operator)