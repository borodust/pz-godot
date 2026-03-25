(common-lisp:in-package :%godot)


(defgmethod
 (method-tweener+set-delay :class 'method-tweener :bind "set_delay" :hash
  266477812)
 method-tweener (delay float))

(defgmethod
 (method-tweener+set-trans :class 'method-tweener :bind "set_trans" :hash
  3740975367)
 method-tweener (trans tween+transition-type))

(defgmethod
 (method-tweener+set-ease :class 'method-tweener :bind "set_ease" :hash
  315540545)
 method-tweener (ease tween+ease-type))