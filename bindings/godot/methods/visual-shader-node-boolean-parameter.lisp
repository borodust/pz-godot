(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-boolean-parameter+set-default-value-enabled :class
  'visual-shader-node-boolean-parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-boolean-parameter+is-default-value-enabled :class
  'visual-shader-node-boolean-parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-boolean-parameter+set-default-value :class
  'visual-shader-node-boolean-parameter :bind "set_default_value" :hash
  2586408642)
 :void (value bool))

(defgmethod
 (visual-shader-node-boolean-parameter+get-default-value :class
  'visual-shader-node-boolean-parameter :bind "get_default_value" :hash
  36873697)
 bool)