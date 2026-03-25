(common-lisp:in-package :%godot)


(defgmethod
 (editor-file-dialog+add-side-menu :class 'editor-file-dialog :bind
  "add_side_menu" :hash 402368861)
 :void (menu control) (title string))

(defgmethod
 (editor-file-dialog+set-disable-overwrite-warning :class 'editor-file-dialog
  :bind "set_disable_overwrite_warning" :hash 2586408642)
 :void (disable bool))

(defgmethod
 (editor-file-dialog+is-overwrite-warning-disabled :class 'editor-file-dialog
  :bind "is_overwrite_warning_disabled" :hash 36873697)
 bool)