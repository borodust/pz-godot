(common-lisp:in-package :%godot)


(defgmethod
 (syntax-highlighter+-get-line-syntax-highlighting :class 'syntax-highlighter
  :bind "_get_line_syntax_highlighting" :hash 3485342025 :virtual
  common-lisp:t)
 dictionary (line int))

(defgmethod
 (syntax-highlighter+-clear-highlighting-cache :class 'syntax-highlighter :bind
  "_clear_highlighting_cache" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (syntax-highlighter+-update-cache :class 'syntax-highlighter :bind
  "_update_cache" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (syntax-highlighter+get-line-syntax-highlighting :class 'syntax-highlighter
  :bind "get_line_syntax_highlighting" :hash 3554694381)
 dictionary (line int))

(defgmethod
 (syntax-highlighter+update-cache :class 'syntax-highlighter :bind
  "update_cache" :hash 3218959716)
 :void)

(defgmethod
 (syntax-highlighter+clear-highlighting-cache :class 'syntax-highlighter :bind
  "clear_highlighting_cache" :hash 3218959716)
 :void)

(defgmethod
 (syntax-highlighter+get-text-edit :class 'syntax-highlighter :bind
  "get_text_edit" :hash 1893027089)
 text-edit)