(common-lisp:in-package :%godot)


(defgmethod
 (omni-light-3d+set-shadow-mode :class 'omni-light-3d :bind "set_shadow_mode"
  :hash 121862228)
 :void (mode omni-light-3d+shadow-mode))

(defgmethod
 (omni-light-3d+get-shadow-mode :class 'omni-light-3d :bind "get_shadow_mode"
  :hash 4181586331)
 omni-light-3d+shadow-mode)