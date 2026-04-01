(common-lisp:in-package :%godot)


(defgmethod
 (editor-syntax-highlighter+%get-name :class 'editor-syntax-highlighter :bind
  "_get_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-syntax-highlighter+%get-supported-languages :class
  'editor-syntax-highlighter :bind "_get_supported_languages" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-syntax-highlighter+%create :class 'editor-syntax-highlighter :bind
  "_create" :hash 3789807118 :virtual common-lisp:t)
 editor-syntax-highlighter)