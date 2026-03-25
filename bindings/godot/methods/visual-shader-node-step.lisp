(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-step+set-op-type :class 'visual-shader-node-step :bind
  "set_op_type" :hash 715172489)
 :void (op-type visual-shader-node-step+op-type))

(defgmethod
 (visual-shader-node-step+get-op-type :class 'visual-shader-node-step :bind
  "get_op_type" :hash 3274022781)
 visual-shader-node-step+op-type)