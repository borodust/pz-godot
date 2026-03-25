(common-lisp:in-package :%godot)


(defgmethod
 (callback-tweener+set-delay :class 'callback-tweener :bind "set_delay" :hash
  3008182292)
 callback-tweener (delay float))