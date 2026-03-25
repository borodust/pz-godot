(common-lisp:in-package :%godot)


(defgproperty button+text 'button :get 'button+get-text :set 'button+set-text)

(defgproperty button+icon 'button :get 'button+get-button-icon :set
 'button+set-button-icon)

(defgproperty button+flat 'button :get 'button+is-flat :set 'button+set-flat)

(defgproperty button+alignment 'button :get 'button+get-text-alignment :set
 'button+set-text-alignment)

(defgproperty button+text-overrun-behavior 'button :get
 'button+get-text-overrun-behavior :set 'button+set-text-overrun-behavior)

(defgproperty button+autowrap-mode 'button :get 'button+get-autowrap-mode :set
 'button+set-autowrap-mode)

(defgproperty button+autowrap-trim-flags 'button :get
 'button+get-autowrap-trim-flags :set 'button+set-autowrap-trim-flags)

(defgproperty button+clip-text 'button :get 'button+get-clip-text :set
 'button+set-clip-text)

(defgproperty button+icon-alignment 'button :get 'button+get-icon-alignment
 :set 'button+set-icon-alignment)

(defgproperty button+vertical-icon-alignment 'button :get
 'button+get-vertical-icon-alignment :set 'button+set-vertical-icon-alignment)

(defgproperty button+expand-icon 'button :get 'button+is-expand-icon :set
 'button+set-expand-icon)

(defgproperty button+text-direction 'button :get 'button+get-text-direction
 :set 'button+set-text-direction)

(defgproperty button+language 'button :get 'button+get-language :set
 'button+set-language)