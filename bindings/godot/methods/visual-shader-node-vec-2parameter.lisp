(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-2parameter+set-default-value-enabled :class
  'visual-shader-node-vec-2parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-vec-2parameter+is-default-value-enabled :class
  'visual-shader-node-vec-2parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-vec-2parameter+set-default-value :class
  'visual-shader-node-vec-2parameter :bind "set_default_value" :hash 743155724)
 :void (value vector-2))

(defgmethod
 (visual-shader-node-vec-2parameter+get-default-value :class
  'visual-shader-node-vec-2parameter :bind "get_default_value" :hash
  3341600327)
 vector-2)