(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-multiply-add+set-op-type :class
  'visual-shader-node-multiply-add :bind "set_op_type" :hash 1409862380)
 :void (type visual-shader-node-multiply-add+op-type))

(defgmethod
 (visual-shader-node-multiply-add+get-op-type :class
  'visual-shader-node-multiply-add :bind "get_op_type" :hash 2823201991)
 visual-shader-node-multiply-add+op-type)