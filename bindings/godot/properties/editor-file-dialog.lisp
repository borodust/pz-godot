(common-lisp:in-package :%godot)


(defgproperty editor-file-dialog+disable-overwrite-warning 'editor-file-dialog
 :get 'editor-file-dialog+is-overwrite-warning-disabled :set
 'editor-file-dialog+set-disable-overwrite-warning)