(common-lisp:in-package :%godot)


(defgproperty translation+messages 'translation)

(defgproperty translation+locale 'translation :get 'translation+get-locale :set
 'translation+set-locale)

(defgproperty translation+plural-rules-override 'translation :get
 'translation+get-plural-rules-override :set
 'translation+set-plural-rules-override)