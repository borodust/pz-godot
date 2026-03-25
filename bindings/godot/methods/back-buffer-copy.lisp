(common-lisp:in-package :%godot)


(defgmethod
 (back-buffer-copy+set-rect :class 'back-buffer-copy :bind "set_rect" :hash
  2046264180)
 :void (rect rect-2))

(defgmethod
 (back-buffer-copy+get-rect :class 'back-buffer-copy :bind "get_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (back-buffer-copy+set-copy-mode :class 'back-buffer-copy :bind "set_copy_mode"
  :hash 1713538590)
 :void (copy-mode back-buffer-copy+copy-mode))

(defgmethod
 (back-buffer-copy+get-copy-mode :class 'back-buffer-copy :bind "get_copy_mode"
  :hash 3271169440)
 back-buffer-copy+copy-mode)