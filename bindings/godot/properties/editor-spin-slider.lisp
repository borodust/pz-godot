(common-lisp:in-package :%godot)


(defgproperty editor-spin-slider+label 'editor-spin-slider :get
 'editor-spin-slider+get-label :set 'editor-spin-slider+set-label)

(defgproperty editor-spin-slider+suffix 'editor-spin-slider :get
 'editor-spin-slider+get-suffix :set 'editor-spin-slider+set-suffix)

(defgproperty editor-spin-slider+read-only 'editor-spin-slider :get
 'editor-spin-slider+is-read-only :set 'editor-spin-slider+set-read-only)

(defgproperty editor-spin-slider+flat 'editor-spin-slider :get
 'editor-spin-slider+is-flat :set 'editor-spin-slider+set-flat)

(defgproperty editor-spin-slider+control-state 'editor-spin-slider :get
 'editor-spin-slider+get-control-state :set
 'editor-spin-slider+set-control-state)

(defgproperty editor-spin-slider+hide-slider 'editor-spin-slider :get
 'editor-spin-slider+is-hiding-slider :set 'editor-spin-slider+set-hide-slider)

(defgproperty editor-spin-slider+editing-integer 'editor-spin-slider :get
 'editor-spin-slider+is-editing-integer :set
 'editor-spin-slider+set-editing-integer)

(defgproperty editor-spin-slider+deferred-drag-mode 'editor-spin-slider :get
 'editor-spin-slider+is-deferred-drag-mode-enabled :set
 'editor-spin-slider+set-deferred-drag-mode-enabled)