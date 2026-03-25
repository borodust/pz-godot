(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-3parameter+set-default-value-enabled :class
  'visual-shader-node-vec-3parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-vec-3parameter+is-default-value-enabled :class
  'visual-shader-node-vec-3parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-vec-3parameter+set-default-value :class
  'visual-shader-node-vec-3parameter :bind "set_default_value" :hash
  3460891852)
 :void (value vector-3))

(defgmethod
 (visual-shader-node-vec-3parameter+get-default-value :class
  'visual-shader-node-vec-3parameter :bind "get_default_value" :hash
  3360562783)
 vector-3)