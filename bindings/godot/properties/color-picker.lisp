(common-lisp:in-package :%godot)


(defgproperty color-picker+color 'color-picker :get
 'color-picker+get-pick-color :set 'color-picker+set-pick-color)

(defgproperty color-picker+edit-alpha 'color-picker :get
 'color-picker+is-editing-alpha :set 'color-picker+set-edit-alpha)

(defgproperty color-picker+edit-intensity 'color-picker :get
 'color-picker+is-editing-intensity :set 'color-picker+set-edit-intensity)

(defgproperty color-picker+color-mode 'color-picker :get
 'color-picker+get-color-mode :set 'color-picker+set-color-mode)

(defgproperty color-picker+deferred-mode 'color-picker :get
 'color-picker+is-deferred-mode :set 'color-picker+set-deferred-mode)

(defgproperty color-picker+picker-shape 'color-picker :get
 'color-picker+get-picker-shape :set 'color-picker+set-picker-shape)

(defgproperty color-picker+can-add-swatches 'color-picker :get
 'color-picker+are-swatches-enabled :set 'color-picker+set-can-add-swatches)

(defgproperty color-picker+sampler-visible 'color-picker :get
 'color-picker+is-sampler-visible :set 'color-picker+set-sampler-visible)

(defgproperty color-picker+color-modes-visible 'color-picker :get
 'color-picker+are-modes-visible :set 'color-picker+set-modes-visible)

(defgproperty color-picker+sliders-visible 'color-picker :get
 'color-picker+are-sliders-visible :set 'color-picker+set-sliders-visible)

(defgproperty color-picker+hex-visible 'color-picker :get
 'color-picker+is-hex-visible :set 'color-picker+set-hex-visible)

(defgproperty color-picker+presets-visible 'color-picker :get
 'color-picker+are-presets-visible :set 'color-picker+set-presets-visible)