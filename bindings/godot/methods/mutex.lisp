(common-lisp:in-package :%godot)


(defgmethod (mutex+lock :class 'mutex :bind "lock" :hash 3218959716) :void)

(defgmethod (mutex+try-lock :class 'mutex :bind "try_lock" :hash 2240911060)
 bool)

(defgmethod (mutex+unlock :class 'mutex :bind "unlock" :hash 3218959716) :void)