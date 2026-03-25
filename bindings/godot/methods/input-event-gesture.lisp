(common-lisp:in-package :%godot)


(defgmethod
 (input-event-gesture+set-position :class 'input-event-gesture :bind
  "set_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (input-event-gesture+get-position :class 'input-event-gesture :bind
  "get_position" :hash 3341600327)
 vector-2)