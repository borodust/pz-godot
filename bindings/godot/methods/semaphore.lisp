(common-lisp:in-package :%godot)


(defgmethod (semaphore+wait :class 'semaphore :bind "wait" :hash 3218959716)
 :void)

(defgmethod
 (semaphore+try-wait :class 'semaphore :bind "try_wait" :hash 2240911060) bool)

(defgmethod (semaphore+post :class 'semaphore :bind "post" :hash 1667783136)
 :void (count int))