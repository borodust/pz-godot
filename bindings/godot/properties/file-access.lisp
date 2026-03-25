(common-lisp:in-package :%godot)


(defgproperty file-access+big-endian 'file-access :get
 'file-access+is-big-endian :set 'file-access+set-big-endian)