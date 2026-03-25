(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-particle-randomness+set-op-type :class
  'visual-shader-node-particle-randomness :bind "set_op_type" :hash 2060089061)
 :void (type visual-shader-node-particle-randomness+op-type))

(defgmethod
 (visual-shader-node-particle-randomness+get-op-type :class
  'visual-shader-node-particle-randomness :bind "get_op_type" :hash 3597061078)
 visual-shader-node-particle-randomness+op-type)