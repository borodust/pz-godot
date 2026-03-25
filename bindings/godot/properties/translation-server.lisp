(common-lisp:in-package :%godot)


(defgproperty translation-server+pseudolocalization-enabled 'translation-server
 :get 'translation-server+is-pseudolocalization-enabled :set
 'translation-server+set-pseudolocalization-enabled)