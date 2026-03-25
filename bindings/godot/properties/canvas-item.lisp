(common-lisp:in-package :%godot)


(defgproperty canvas-item+visible 'canvas-item :get 'canvas-item+is-visible
 :set 'canvas-item+set-visible)

(defgproperty canvas-item+modulate 'canvas-item :get 'canvas-item+get-modulate
 :set 'canvas-item+set-modulate)

(defgproperty canvas-item+self-modulate 'canvas-item :get
 'canvas-item+get-self-modulate :set 'canvas-item+set-self-modulate)

(defgproperty canvas-item+show-behind-parent 'canvas-item :get
 'canvas-item+is-draw-behind-parent-enabled :set
 'canvas-item+set-draw-behind-parent)

(defgproperty canvas-item+top-level 'canvas-item :get
 'canvas-item+is-set-as-top-level :set 'canvas-item+set-as-top-level)

(defgproperty canvas-item+clip-children 'canvas-item :get
 'canvas-item+get-clip-children-mode :set 'canvas-item+set-clip-children-mode)

(defgproperty canvas-item+light-mask 'canvas-item :get
 'canvas-item+get-light-mask :set 'canvas-item+set-light-mask)

(defgproperty canvas-item+visibility-layer 'canvas-item :get
 'canvas-item+get-visibility-layer :set 'canvas-item+set-visibility-layer)

(defgproperty canvas-item+z-index 'canvas-item :get 'canvas-item+get-z-index
 :set 'canvas-item+set-z-index)

(defgproperty canvas-item+z-as-relative 'canvas-item :get
 'canvas-item+is-z-relative :set 'canvas-item+set-z-as-relative)

(defgproperty canvas-item+y-sort-enabled 'canvas-item :get
 'canvas-item+is-y-sort-enabled :set 'canvas-item+set-y-sort-enabled)

(defgproperty canvas-item+texture-filter 'canvas-item :get
 'canvas-item+get-texture-filter :set 'canvas-item+set-texture-filter)

(defgproperty canvas-item+texture-repeat 'canvas-item :get
 'canvas-item+get-texture-repeat :set 'canvas-item+set-texture-repeat)

(defgproperty canvas-item+material 'canvas-item :get 'canvas-item+get-material
 :set 'canvas-item+set-material)

(defgproperty canvas-item+use-parent-material 'canvas-item :get
 'canvas-item+get-use-parent-material :set 'canvas-item+set-use-parent-material)