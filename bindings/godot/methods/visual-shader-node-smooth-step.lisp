(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-smooth-step+set-op-type :class
  'visual-shader-node-smooth-step :bind "set_op_type" :hash 2427426148)
 :void (op-type visual-shader-node-smooth-step+op-type))

(defgmethod
 (visual-shader-node-smooth-step+get-op-type :class
  'visual-shader-node-smooth-step :bind "get_op_type" :hash 359640855)
 visual-shader-node-smooth-step+op-type)