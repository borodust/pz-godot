(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-time-seek+set-explicit-elapse :class 'animation-node-time-seek
  :bind "set_explicit_elapse" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-time-seek+is-explicit-elapse :class 'animation-node-time-seek
  :bind "is_explicit_elapse" :hash 36873697)
 bool)