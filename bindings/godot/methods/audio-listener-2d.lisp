(common-lisp:in-package :%godot)


(defgmethod
 (audio-listener-2d+make-current :class 'audio-listener-2d :bind "make_current"
  :hash 3218959716)
 :void)

(defgmethod
 (audio-listener-2d+clear-current :class 'audio-listener-2d :bind
  "clear_current" :hash 3218959716)
 :void)

(defgmethod
 (audio-listener-2d+is-current :class 'audio-listener-2d :bind "is_current"
  :hash 36873697)
 bool)