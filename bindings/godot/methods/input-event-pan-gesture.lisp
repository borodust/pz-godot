(common-lisp:in-package :%godot)


(defgmethod
 (input-event-pan-gesture+set-delta :class 'input-event-pan-gesture :bind
  "set_delta" :hash 743155724)
 :void (delta vector-2))

(defgmethod
 (input-event-pan-gesture+get-delta :class 'input-event-pan-gesture :bind
  "get_delta" :hash 3341600327)
 vector-2)