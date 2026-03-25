(common-lisp:in-package :%godot)


(defgmethod
 (color-palette+set-colors :class 'color-palette :bind "set_colors" :hash
  3546319833)
 :void (colors packed-color-array))

(defgmethod
 (color-palette+get-colors :class 'color-palette :bind "get_colors" :hash
  1392750486)
 packed-color-array)