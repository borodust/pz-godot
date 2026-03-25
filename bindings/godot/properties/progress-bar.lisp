(common-lisp:in-package :%godot)


(defgproperty progress-bar+fill-mode 'progress-bar :get
 'progress-bar+get-fill-mode :set 'progress-bar+set-fill-mode)

(defgproperty progress-bar+show-percentage 'progress-bar :get
 'progress-bar+is-percentage-shown :set 'progress-bar+set-show-percentage)

(defgproperty progress-bar+indeterminate 'progress-bar :get
 'progress-bar+is-indeterminate :set 'progress-bar+set-indeterminate)

(defgproperty progress-bar+editor-preview-indeterminate 'progress-bar :get
 'progress-bar+is-editor-preview-indeterminate-enabled :set
 'progress-bar+set-editor-preview-indeterminate)