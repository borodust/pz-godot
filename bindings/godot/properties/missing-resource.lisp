(common-lisp:in-package :%godot)


(defgproperty missing-resource+original-class 'missing-resource :get
 'missing-resource+get-original-class :set 'missing-resource+set-original-class)

(defgproperty missing-resource+recording-properties 'missing-resource :get
 'missing-resource+is-recording-properties :set
 'missing-resource+set-recording-properties)