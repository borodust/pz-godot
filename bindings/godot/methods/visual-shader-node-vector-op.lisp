(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vector-op+set-operator :class
  'visual-shader-node-vector-op :bind "set_operator" :hash 3371507302)
 :void (op visual-shader-node-vector-op+operator))

(defgmethod
 (visual-shader-node-vector-op+get-operator :class
  'visual-shader-node-vector-op :bind "get_operator" :hash 11793929)
 visual-shader-node-vector-op+operator)