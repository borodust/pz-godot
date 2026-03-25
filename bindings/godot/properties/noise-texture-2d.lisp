(common-lisp:in-package :%godot)


(defgproperty noise-texture-2d+width 'noise-texture-2d :set
 'noise-texture-2d+set-width)

(defgproperty noise-texture-2d+height 'noise-texture-2d :set
 'noise-texture-2d+set-height)

(defgproperty noise-texture-2d+generate-mipmaps 'noise-texture-2d :get
 'noise-texture-2d+is-generating-mipmaps :set
 'noise-texture-2d+set-generate-mipmaps)

(defgproperty noise-texture-2d+noise 'noise-texture-2d :get
 'noise-texture-2d+get-noise :set 'noise-texture-2d+set-noise)

(defgproperty noise-texture-2d+color-ramp 'noise-texture-2d :get
 'noise-texture-2d+get-color-ramp :set 'noise-texture-2d+set-color-ramp)

(defgproperty noise-texture-2d+seamless 'noise-texture-2d :get
 'noise-texture-2d+get-seamless :set 'noise-texture-2d+set-seamless)

(defgproperty noise-texture-2d+invert 'noise-texture-2d :get
 'noise-texture-2d+get-invert :set 'noise-texture-2d+set-invert)

(defgproperty noise-texture-2d+in-3d-space 'noise-texture-2d :get
 'noise-texture-2d+is-in-3d-space :set 'noise-texture-2d+set-in-3d-space)

(defgproperty noise-texture-2d+as-normal-map 'noise-texture-2d :get
 'noise-texture-2d+is-normal-map :set 'noise-texture-2d+set-as-normal-map)

(defgproperty noise-texture-2d+normalize 'noise-texture-2d :get
 'noise-texture-2d+is-normalized :set 'noise-texture-2d+set-normalize)

(defgproperty noise-texture-2d+seamless-blend-skirt 'noise-texture-2d :get
 'noise-texture-2d+get-seamless-blend-skirt :set
 'noise-texture-2d+set-seamless-blend-skirt)

(defgproperty noise-texture-2d+bump-strength 'noise-texture-2d :get
 'noise-texture-2d+get-bump-strength :set 'noise-texture-2d+set-bump-strength)