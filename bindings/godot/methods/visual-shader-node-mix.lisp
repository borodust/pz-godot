(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-mix+set-op-type :class 'visual-shader-node-mix :bind
  "set_op_type" :hash 3397501671)
 :void (op-type visual-shader-node-mix+op-type))

(defgmethod
 (visual-shader-node-mix+get-op-type :class 'visual-shader-node-mix :bind
  "get_op_type" :hash 4013957297)
 visual-shader-node-mix+op-type)