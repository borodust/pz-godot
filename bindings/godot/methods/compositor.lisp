(common-lisp:in-package :%godot)


(defgmethod
 (compositor+set-compositor-effects :class 'compositor :bind
  "set_compositor_effects" :hash 381264803)
 :void (compositor-effects array))

(defgmethod
 (compositor+get-compositor-effects :class 'compositor :bind
  "get_compositor_effects" :hash 3995934104)
 array)