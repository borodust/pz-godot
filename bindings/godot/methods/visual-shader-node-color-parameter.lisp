(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-color-parameter+set-default-value-enabled :class
  'visual-shader-node-color-parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-color-parameter+is-default-value-enabled :class
  'visual-shader-node-color-parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-color-parameter+set-default-value :class
  'visual-shader-node-color-parameter :bind "set_default_value" :hash
  2920490490)
 :void (value color))

(defgmethod
 (visual-shader-node-color-parameter+get-default-value :class
  'visual-shader-node-color-parameter :bind "get_default_value" :hash
  3444240500)
 color)