(common-lisp:in-package :%godot)


(defgmethod
 (gradient-texture-1d+set-gradient :class 'gradient-texture-1d :bind
  "set_gradient" :hash 2756054477)
 :void (gradient gradient))

(defgmethod
 (gradient-texture-1d+get-gradient :class 'gradient-texture-1d :bind
  "get_gradient" :hash 132272999)
 gradient)

(defgmethod
 (gradient-texture-1d+set-width :class 'gradient-texture-1d :bind "set_width"
  :hash 1286410249)
 :void (width int))

(defgmethod
 (gradient-texture-1d+set-use-hdr :class 'gradient-texture-1d :bind
  "set_use_hdr" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (gradient-texture-1d+is-using-hdr :class 'gradient-texture-1d :bind
  "is_using_hdr" :hash 36873697)
 bool)