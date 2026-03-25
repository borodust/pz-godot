(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vector-base+set-op-type :class
  'visual-shader-node-vector-base :bind "set_op_type" :hash 1692596998)
 :void (type visual-shader-node-vector-base+op-type))

(defgmethod
 (visual-shader-node-vector-base+get-op-type :class
  'visual-shader-node-vector-base :bind "get_op_type" :hash 2568738462)
 visual-shader-node-vector-base+op-type)