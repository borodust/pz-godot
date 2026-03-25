(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-transform-vec-mult+set-operator :class
  'visual-shader-node-transform-vec-mult :bind "set_operator" :hash 1785665912)
 :void (op visual-shader-node-transform-vec-mult+operator))

(defgmethod
 (visual-shader-node-transform-vec-mult+get-operator :class
  'visual-shader-node-transform-vec-mult :bind "get_operator" :hash 1622088722)
 visual-shader-node-transform-vec-mult+operator)