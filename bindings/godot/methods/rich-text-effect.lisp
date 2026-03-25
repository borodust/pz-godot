(common-lisp:in-package :%godot)


(defgmethod
 (rich-text-effect+-process-custom-fx :class 'rich-text-effect :bind
  "_process_custom_fx" :hash 31984339 :virtual common-lisp:t)
 bool (char-fx char-fxtransform))