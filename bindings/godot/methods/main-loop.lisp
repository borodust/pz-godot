(common-lisp:in-package :%godot)


(defgmethod
 (main-loop+%initialize :class 'main-loop :bind "_initialize" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (main-loop+%physics-process :class 'main-loop :bind "_physics_process" :hash
  330693286 :virtual common-lisp:t)
 bool (delta float))

(defgmethod
 (main-loop+%process :class 'main-loop :bind "_process" :hash 330693286
  :virtual common-lisp:t)
 bool (delta float))

(defgmethod
 (main-loop+%finalize :class 'main-loop :bind "_finalize" :hash 3218959716
  :virtual common-lisp:t)
 :void)