(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-uint-parameter+set-default-value-enabled :class
  'visual-shader-node-uint-parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-uint-parameter+is-default-value-enabled :class
  'visual-shader-node-uint-parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-uint-parameter+set-default-value :class
  'visual-shader-node-uint-parameter :bind "set_default_value" :hash
  1286410249)
 :void (value int))

(defgmethod
 (visual-shader-node-uint-parameter+get-default-value :class
  'visual-shader-node-uint-parameter :bind "get_default_value" :hash
  3905245786)
 int)