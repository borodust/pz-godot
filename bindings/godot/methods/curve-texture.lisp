(common-lisp:in-package :%godot)


(defgmethod
 (curve-texture+set-width :class 'curve-texture :bind "set_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (curve-texture+set-curve :class 'curve-texture :bind "set_curve" :hash
  270443179)
 :void (curve curve))

(defgmethod
 (curve-texture+get-curve :class 'curve-texture :bind "get_curve" :hash
  2460114913)
 curve)

(defgmethod
 (curve-texture+set-texture-mode :class 'curve-texture :bind "set_texture_mode"
  :hash 1321955367)
 :void (texture-mode curve-texture+texture-mode))

(defgmethod
 (curve-texture+get-texture-mode :class 'curve-texture :bind "get_texture_mode"
  :hash 715756376)
 curve-texture+texture-mode)