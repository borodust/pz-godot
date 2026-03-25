(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-remap+set-op-type :class 'visual-shader-node-remap :bind
  "set_op_type" :hash 1703697889)
 :void (op-type visual-shader-node-remap+op-type))

(defgmethod
 (visual-shader-node-remap+get-op-type :class 'visual-shader-node-remap :bind
  "get_op_type" :hash 1678380563)
 visual-shader-node-remap+op-type)