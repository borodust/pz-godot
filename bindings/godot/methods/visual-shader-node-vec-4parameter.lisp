(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-vec-4parameter+set-default-value-enabled :class
  'visual-shader-node-vec-4parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-vec-4parameter+is-default-value-enabled :class
  'visual-shader-node-vec-4parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-vec-4parameter+set-default-value :class
  'visual-shader-node-vec-4parameter :bind "set_default_value" :hash 643568085)
 :void (value vector-4))

(defgmethod
 (visual-shader-node-vec-4parameter+get-default-value :class
  'visual-shader-node-vec-4parameter :bind "get_default_value" :hash
  2435802345)
 vector-4)