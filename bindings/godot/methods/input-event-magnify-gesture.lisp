(common-lisp:in-package :%godot)


(defgmethod
 (input-event-magnify-gesture+set-factor :class 'input-event-magnify-gesture
  :bind "set_factor" :hash 373806689)
 :void (factor float))

(defgmethod
 (input-event-magnify-gesture+get-factor :class 'input-event-magnify-gesture
  :bind "get_factor" :hash 1740695150)
 float)