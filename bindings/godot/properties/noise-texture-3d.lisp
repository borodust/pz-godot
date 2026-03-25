(common-lisp:in-package :%godot)


(defgproperty noise-texture-3d+width 'noise-texture-3d :set
 'noise-texture-3d+set-width)

(defgproperty noise-texture-3d+height 'noise-texture-3d :set
 'noise-texture-3d+set-height)

(defgproperty noise-texture-3d+depth 'noise-texture-3d :set
 'noise-texture-3d+set-depth)

(defgproperty noise-texture-3d+noise 'noise-texture-3d :get
 'noise-texture-3d+get-noise :set 'noise-texture-3d+set-noise)

(defgproperty noise-texture-3d+color-ramp 'noise-texture-3d :get
 'noise-texture-3d+get-color-ramp :set 'noise-texture-3d+set-color-ramp)

(defgproperty noise-texture-3d+seamless 'noise-texture-3d :get
 'noise-texture-3d+get-seamless :set 'noise-texture-3d+set-seamless)

(defgproperty noise-texture-3d+invert 'noise-texture-3d :get
 'noise-texture-3d+get-invert :set 'noise-texture-3d+set-invert)

(defgproperty noise-texture-3d+normalize 'noise-texture-3d :get
 'noise-texture-3d+is-normalized :set 'noise-texture-3d+set-normalize)

(defgproperty noise-texture-3d+seamless-blend-skirt 'noise-texture-3d :get
 'noise-texture-3d+get-seamless-blend-skirt :set
 'noise-texture-3d+set-seamless-blend-skirt)