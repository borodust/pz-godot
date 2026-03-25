(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-transform-parameter+set-default-value-enabled :class
  'visual-shader-node-transform-parameter :bind "set_default_value_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-transform-parameter+is-default-value-enabled :class
  'visual-shader-node-transform-parameter :bind "is_default_value_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (visual-shader-node-transform-parameter+set-default-value :class
  'visual-shader-node-transform-parameter :bind "set_default_value" :hash
  2952846383)
 :void (value transform-3d))

(defgmethod
 (visual-shader-node-transform-parameter+get-default-value :class
  'visual-shader-node-transform-parameter :bind "get_default_value" :hash
  3229777777)
 transform-3d)