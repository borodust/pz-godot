(common-lisp:in-package :%godot)


(defgmethod
 (curve-xyztexture+set-width :class 'curve-xyztexture :bind "set_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (curve-xyztexture+set-curve-x :class 'curve-xyztexture :bind "set_curve_x"
  :hash 270443179)
 :void (curve curve))

(defgmethod
 (curve-xyztexture+get-curve-x :class 'curve-xyztexture :bind "get_curve_x"
  :hash 2460114913)
 curve)

(defgmethod
 (curve-xyztexture+set-curve-y :class 'curve-xyztexture :bind "set_curve_y"
  :hash 270443179)
 :void (curve curve))

(defgmethod
 (curve-xyztexture+get-curve-y :class 'curve-xyztexture :bind "get_curve_y"
  :hash 2460114913)
 curve)

(defgmethod
 (curve-xyztexture+set-curve-z :class 'curve-xyztexture :bind "set_curve_z"
  :hash 270443179)
 :void (curve curve))

(defgmethod
 (curve-xyztexture+get-curve-z :class 'curve-xyztexture :bind "get_curve_z"
  :hash 2460114913)
 curve)