(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-sync+set-use-sync :class 'animation-node-sync :bind
  "set_use_sync" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-sync+is-using-sync :class 'animation-node-sync :bind
  "is_using_sync" :hash 36873697)
 bool)