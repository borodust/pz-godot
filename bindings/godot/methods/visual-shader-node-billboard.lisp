(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-billboard+set-billboard-type :class
  'visual-shader-node-billboard :bind "set_billboard_type" :hash 1227463289)
 :void (billboard-type visual-shader-node-billboard+billboard-type))

(defgmethod
 (visual-shader-node-billboard+get-billboard-type :class
  'visual-shader-node-billboard :bind "get_billboard_type" :hash 3724188517)
 visual-shader-node-billboard+billboard-type)

(defgmethod
 (visual-shader-node-billboard+set-keep-scale-enabled :class
  'visual-shader-node-billboard :bind "set_keep_scale_enabled" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-billboard+is-keep-scale-enabled :class
  'visual-shader-node-billboard :bind "is_keep_scale_enabled" :hash 36873697)
 bool)