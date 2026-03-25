(common-lisp:in-package :%godot)


(defgmethod
 (scroll-bar+set-custom-step :class 'scroll-bar :bind "set_custom_step" :hash
  373806689)
 :void (step float))

(defgmethod
 (scroll-bar+get-custom-step :class 'scroll-bar :bind "get_custom_step" :hash
  1740695150)
 float)