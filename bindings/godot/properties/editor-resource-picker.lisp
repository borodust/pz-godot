(common-lisp:in-package :%godot)


(defgproperty editor-resource-picker+base-type 'editor-resource-picker :get
 'editor-resource-picker+get-base-type :set
 'editor-resource-picker+set-base-type)

(defgproperty editor-resource-picker+edited-resource 'editor-resource-picker
 :get 'editor-resource-picker+get-edited-resource :set
 'editor-resource-picker+set-edited-resource)

(defgproperty editor-resource-picker+editable 'editor-resource-picker :get
 'editor-resource-picker+is-editable :set 'editor-resource-picker+set-editable)

(defgproperty editor-resource-picker+toggle-mode 'editor-resource-picker :get
 'editor-resource-picker+is-toggle-mode :set
 'editor-resource-picker+set-toggle-mode)