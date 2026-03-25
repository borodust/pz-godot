(common-lisp:in-package :%godot)


(defgmethod
 (progress-bar+set-fill-mode :class 'progress-bar :bind "set_fill_mode" :hash
  1286410249)
 :void (mode int))

(defgmethod
 (progress-bar+get-fill-mode :class 'progress-bar :bind "get_fill_mode" :hash
  2455072627)
 int)

(defgmethod
 (progress-bar+set-show-percentage :class 'progress-bar :bind
  "set_show_percentage" :hash 2586408642)
 :void (visible bool))

(defgmethod
 (progress-bar+is-percentage-shown :class 'progress-bar :bind
  "is_percentage_shown" :hash 36873697)
 bool)

(defgmethod
 (progress-bar+set-indeterminate :class 'progress-bar :bind "set_indeterminate"
  :hash 2586408642)
 :void (indeterminate bool))

(defgmethod
 (progress-bar+is-indeterminate :class 'progress-bar :bind "is_indeterminate"
  :hash 36873697)
 bool)

(defgmethod
 (progress-bar+set-editor-preview-indeterminate :class 'progress-bar :bind
  "set_editor_preview_indeterminate" :hash 2586408642)
 :void (preview-indeterminate bool))

(defgmethod
 (progress-bar+is-editor-preview-indeterminate-enabled :class 'progress-bar
  :bind "is_editor_preview_indeterminate_enabled" :hash 36873697)
 bool)