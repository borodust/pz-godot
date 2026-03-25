(common-lisp:in-package :%godot)


(defgmethod
 (canvas-group+set-fit-margin :class 'canvas-group :bind "set_fit_margin" :hash
  373806689)
 :void (fit-margin float))

(defgmethod
 (canvas-group+get-fit-margin :class 'canvas-group :bind "get_fit_margin" :hash
  1740695150)
 float)

(defgmethod
 (canvas-group+set-clear-margin :class 'canvas-group :bind "set_clear_margin"
  :hash 373806689)
 :void (clear-margin float))

(defgmethod
 (canvas-group+get-clear-margin :class 'canvas-group :bind "get_clear_margin"
  :hash 1740695150)
 float)

(defgmethod
 (canvas-group+set-use-mipmaps :class 'canvas-group :bind "set_use_mipmaps"
  :hash 2586408642)
 :void (use-mipmaps bool))

(defgmethod
 (canvas-group+is-using-mipmaps :class 'canvas-group :bind "is_using_mipmaps"
  :hash 36873697)
 bool)