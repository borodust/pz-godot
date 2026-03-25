(common-lisp:in-package :%godot)


(defgmethod
 (gradient-texture-2d+set-gradient :class 'gradient-texture-2d :bind
  "set_gradient" :hash 2756054477)
 :void (gradient gradient))

(defgmethod
 (gradient-texture-2d+get-gradient :class 'gradient-texture-2d :bind
  "get_gradient" :hash 132272999)
 gradient)

(defgmethod
 (gradient-texture-2d+set-width :class 'gradient-texture-2d :bind "set_width"
  :hash 1286410249)
 :void (width int))

(defgmethod
 (gradient-texture-2d+set-height :class 'gradient-texture-2d :bind "set_height"
  :hash 1286410249)
 :void (height int))

(defgmethod
 (gradient-texture-2d+set-use-hdr :class 'gradient-texture-2d :bind
  "set_use_hdr" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (gradient-texture-2d+is-using-hdr :class 'gradient-texture-2d :bind
  "is_using_hdr" :hash 36873697)
 bool)

(defgmethod
 (gradient-texture-2d+set-fill :class 'gradient-texture-2d :bind "set_fill"
  :hash 3623927636)
 :void (fill gradient-texture-2d+fill))

(defgmethod
 (gradient-texture-2d+get-fill :class 'gradient-texture-2d :bind "get_fill"
  :hash 1876227217)
 gradient-texture-2d+fill)

(defgmethod
 (gradient-texture-2d+set-fill-from :class 'gradient-texture-2d :bind
  "set_fill_from" :hash 743155724)
 :void (fill-from vector-2))

(defgmethod
 (gradient-texture-2d+get-fill-from :class 'gradient-texture-2d :bind
  "get_fill_from" :hash 3341600327)
 vector-2)

(defgmethod
 (gradient-texture-2d+set-fill-to :class 'gradient-texture-2d :bind
  "set_fill_to" :hash 743155724)
 :void (fill-to vector-2))

(defgmethod
 (gradient-texture-2d+get-fill-to :class 'gradient-texture-2d :bind
  "get_fill_to" :hash 3341600327)
 vector-2)

(defgmethod
 (gradient-texture-2d+set-repeat :class 'gradient-texture-2d :bind "set_repeat"
  :hash 1357597002)
 :void (repeat gradient-texture-2d+repeat))

(defgmethod
 (gradient-texture-2d+get-repeat :class 'gradient-texture-2d :bind "get_repeat"
  :hash 3351758665)
 gradient-texture-2d+repeat)