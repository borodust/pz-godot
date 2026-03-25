(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-int-parameter+set-hint :class
  'visual-shader-node-int-parameter :bind "set_hint" :hash 2540512075)
 :void (hint visual-shader-node-int-parameter+hint))

(defgmethod
 (visual-shader-node-int-parameter+get-hint :class
  'visual-shader-node-int-parameter :bind "get_hint" :hash 4250814924)
 visual-shader-node-int-parameter+hint)

(defgmethod
 (visual-shader-node-int-parameter+set-min :class
  'visual-shader-node-int-parameter :bind "set_min" :hash 1286410249)
 :void (value int))

(defgmethod
 (visual-shader-node-int-parameter+get-min :class
  'visual-shader-node-int-parameter :bind "get_min" :hash 3905245786)
 int)

(defgmethod
 (visual-shader-node-int-parameter+set-max :class
  'visual-shader-node-int-parameter :bind "set_max" :hash 1286410249)
 :void (value int))

(defgmethod
 (visual-shader-node-int-parameter+get-max :class
  'visual-shader-node-int-parameter :bind "get_max" :hash 3905245786)
 int)

(defgmethod
 (visual-shader-node-int-parameter+set-step :class
  'visual-shader-node-int-parameter :bind "set_step" :hash 1286410249)
 :void (value int))

(defgmethod
 (visual-shader-node-int-parameter+get-step :class
  'visual-shader-node-int-parameter :bind "get_step" :hash 3905245786)
 int)

(defgmethod
 (visual-shader-node-int-parameter+set-enum-names :class
  'visual-shader-node-int-parameter :bind "set_enum_names" :hash 4015028928)
 :void (names packed-string-array))

(defgmethod
 (visual-shader-node-int-parameter+get-enum-names :class
  'visual-shader-node-int-parameter :bind "get_enum_names" :hash 1139954409)
 packed-string-array)

(defgmethod
 (visual-shader-node-int-parameter+set-default-value-enabled :class
  'visual-shader-node-int-parameter :bind "set_default_value_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-int-parameter+is-default-value-enabled :class
  'visual-shader-node-int-parameter :bind "is_default_value_enabled" :hash
  36873697)
 bool)

(defgmethod
 (visual-shader-node-int-parameter+set-default-value :class
  'visual-shader-node-int-parameter :bind "set_default_value" :hash 1286410249)
 :void (value int))

(defgmethod
 (visual-shader-node-int-parameter+get-default-value :class
  'visual-shader-node-int-parameter :bind "get_default_value" :hash 3905245786)
 int)