(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-float-parameter+set-hint :class
  'visual-shader-node-float-parameter :bind "set_hint" :hash 3712586466)
 :void (hint visual-shader-node-float-parameter+hint))

(defgmethod
 (visual-shader-node-float-parameter+get-hint :class
  'visual-shader-node-float-parameter :bind "get_hint" :hash 3042240429)
 visual-shader-node-float-parameter+hint)

(defgmethod
 (visual-shader-node-float-parameter+set-min :class
  'visual-shader-node-float-parameter :bind "set_min" :hash 373806689)
 :void (value float))

(defgmethod
 (visual-shader-node-float-parameter+get-min :class
  'visual-shader-node-float-parameter :bind "get_min" :hash 1740695150)
 float)

(defgmethod
 (visual-shader-node-float-parameter+set-max :class
  'visual-shader-node-float-parameter :bind "set_max" :hash 373806689)
 :void (value float))

(defgmethod
 (visual-shader-node-float-parameter+get-max :class
  'visual-shader-node-float-parameter :bind "get_max" :hash 1740695150)
 float)

(defgmethod
 (visual-shader-node-float-parameter+set-step :class
  'visual-shader-node-float-parameter :bind "set_step" :hash 373806689)
 :void (value float))

(defgmethod
 (visual-shader-node-float-parameter+get-step :class
  'visual-shader-node-float-parameter :bind "get_step" :hash 1740695150)
 float)

(defgmethod
 (visual-shader-node-float-parameter+set-default-value-enabled :class
  'visual-shader-node-float-parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-float-parameter+is-default-value-enabled :class
  'visual-shader-node-float-parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-float-parameter+set-default-value :class
  'visual-shader-node-float-parameter :bind "set_default_value" :hash
  373806689)
 :void (value float))

(defgmethod
 (visual-shader-node-float-parameter+get-default-value :class
  'visual-shader-node-float-parameter :bind "get_default_value" :hash
  1740695150)
 float)