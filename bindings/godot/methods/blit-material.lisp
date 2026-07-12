(common-lisp:in-package :%godot)


(defgmethod
 (blit-material+set-blend-mode :class 'blit-material :bind "set_blend_mode"
  :hash 80206916)
 :void (blend-mode blit-material+blend-mode))

(defgmethod
 (blit-material+get-blend-mode :class 'blit-material :bind "get_blend_mode"
  :hash 4234246416)
 blit-material+blend-mode)