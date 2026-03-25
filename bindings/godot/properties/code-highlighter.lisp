(common-lisp:in-package :%godot)


(defgproperty code-highlighter+number-color 'code-highlighter :get
 'code-highlighter+get-number-color :set 'code-highlighter+set-number-color)

(defgproperty code-highlighter+symbol-color 'code-highlighter :get
 'code-highlighter+get-symbol-color :set 'code-highlighter+set-symbol-color)

(defgproperty code-highlighter+function-color 'code-highlighter :get
 'code-highlighter+get-function-color :set 'code-highlighter+set-function-color)

(defgproperty code-highlighter+member-variable-color 'code-highlighter :get
 'code-highlighter+get-member-variable-color :set
 'code-highlighter+set-member-variable-color)

(defgproperty code-highlighter+keyword-colors 'code-highlighter :get
 'code-highlighter+get-keyword-colors :set 'code-highlighter+set-keyword-colors)

(defgproperty code-highlighter+member-keyword-colors 'code-highlighter :get
 'code-highlighter+get-member-keyword-colors :set
 'code-highlighter+set-member-keyword-colors)

(defgproperty code-highlighter+color-regions 'code-highlighter :get
 'code-highlighter+get-color-regions :set 'code-highlighter+set-color-regions)