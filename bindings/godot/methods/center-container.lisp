(common-lisp:in-package :%godot)


(defgmethod
 (center-container+set-use-top-left :class 'center-container :bind
  "set_use_top_left" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (center-container+is-using-top-left :class 'center-container :bind
  "is_using_top_left" :hash 36873697)
 bool)