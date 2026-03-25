(common-lisp:in-package :%godot)


(defgproperty color-picker-button+color 'color-picker-button :get
 'color-picker-button+get-pick-color :set 'color-picker-button+set-pick-color)

(defgproperty color-picker-button+edit-alpha 'color-picker-button :get
 'color-picker-button+is-editing-alpha :set 'color-picker-button+set-edit-alpha)

(defgproperty color-picker-button+edit-intensity 'color-picker-button :get
 'color-picker-button+is-editing-intensity :set
 'color-picker-button+set-edit-intensity)