(common-lisp:in-package :%godot)


(defgmethod
 (canvas-modulate+set-color :class 'canvas-modulate :bind "set_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (canvas-modulate+get-color :class 'canvas-modulate :bind "get_color" :hash
  3444240500)
 color)