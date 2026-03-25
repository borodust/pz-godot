(common-lisp:in-package :%godot)


(defgmethod
 (color-rect+set-color :class 'color-rect :bind "set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (color-rect+get-color :class 'color-rect :bind "get_color" :hash 3444240500)
 color)