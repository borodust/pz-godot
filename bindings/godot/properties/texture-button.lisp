(common-lisp:in-package :%godot)


(defgproperty texture-button+texture-normal 'texture-button :get
 'texture-button+get-texture-normal :set 'texture-button+set-texture-normal)

(defgproperty texture-button+texture-pressed 'texture-button :get
 'texture-button+get-texture-pressed :set 'texture-button+set-texture-pressed)

(defgproperty texture-button+texture-hover 'texture-button :get
 'texture-button+get-texture-hover :set 'texture-button+set-texture-hover)

(defgproperty texture-button+texture-disabled 'texture-button :get
 'texture-button+get-texture-disabled :set 'texture-button+set-texture-disabled)

(defgproperty texture-button+texture-focused 'texture-button :get
 'texture-button+get-texture-focused :set 'texture-button+set-texture-focused)

(defgproperty texture-button+texture-click-mask 'texture-button :get
 'texture-button+get-click-mask :set 'texture-button+set-click-mask)

(defgproperty texture-button+ignore-texture-size 'texture-button :get
 'texture-button+get-ignore-texture-size :set
 'texture-button+set-ignore-texture-size)

(defgproperty texture-button+stretch-mode 'texture-button :get
 'texture-button+get-stretch-mode :set 'texture-button+set-stretch-mode)

(defgproperty texture-button+flip-h 'texture-button :get
 'texture-button+is-flipped-h :set 'texture-button+set-flip-h)

(defgproperty texture-button+flip-v 'texture-button :get
 'texture-button+is-flipped-v :set 'texture-button+set-flip-v)