(common-lisp:in-package :%godot)


(defgmethod
 (visual-shader-node-particle-emitter+set-mode-2d :class
  'visual-shader-node-particle-emitter :bind "set_mode_2d" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (visual-shader-node-particle-emitter+is-mode-2d :class
  'visual-shader-node-particle-emitter :bind "is_mode_2d" :hash 36873697)
 bool)