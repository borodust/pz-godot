(common-lisp:in-package :%godot)


(defgproperty stream-peer+big-endian 'stream-peer :get
 'stream-peer+is-big-endian-enabled :set 'stream-peer+set-big-endian)