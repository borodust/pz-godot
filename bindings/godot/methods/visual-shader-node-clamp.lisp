(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-clamp+set-op-type :class 'visual-shader-node-clamp :bind
  "set_op_type" :hash 405010749)
 :void (op-type visual-shader-node-clamp+op-type))

(defgmethod
 (visual-shader-node-clamp+get-op-type :class 'visual-shader-node-clamp :bind
  "get_op_type" :hash 233276050)
 visual-shader-node-clamp+op-type)