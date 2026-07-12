(common-lisp:in-package :%godot)


(defgmethod
 (await-tweener+set-timeout :class 'await-tweener :bind "set_timeout" :hash
  3123469156)
 await-tweener (timeout float))