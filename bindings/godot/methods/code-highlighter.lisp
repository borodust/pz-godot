(common-lisp:in-package :%godot)


(defgmethod
 (code-highlighter+add-keyword-color :class 'code-highlighter :bind
  "add_keyword_color" :hash 1636512886)
 :void (keyword string) (color color))

(defgmethod
 (code-highlighter+remove-keyword-color :class 'code-highlighter :bind
  "remove_keyword_color" :hash 83702148)
 :void (keyword string))

(defgmethod
 (code-highlighter+has-keyword-color :class 'code-highlighter :bind
  "has_keyword_color" :hash 3927539163)
 bool (keyword string))

(defgmethod
 (code-highlighter+get-keyword-color :class 'code-highlighter :bind
  "get_keyword_color" :hash 3855908743)
 color (keyword string))

(defgmethod
 (code-highlighter+set-keyword-colors :class 'code-highlighter :bind
  "set_keyword_colors" :hash 4155329257)
 :void (keywords dictionary))

(defgmethod
 (code-highlighter+clear-keyword-colors :class 'code-highlighter :bind
  "clear_keyword_colors" :hash 3218959716)
 :void)

(defgmethod
 (code-highlighter+get-keyword-colors :class 'code-highlighter :bind
  "get_keyword_colors" :hash 3102165223)
 dictionary)

(defgmethod
 (code-highlighter+add-member-keyword-color :class 'code-highlighter :bind
  "add_member_keyword_color" :hash 1636512886)
 :void (member-keyword string) (color color))

(defgmethod
 (code-highlighter+remove-member-keyword-color :class 'code-highlighter :bind
  "remove_member_keyword_color" :hash 83702148)
 :void (member-keyword string))

(defgmethod
 (code-highlighter+has-member-keyword-color :class 'code-highlighter :bind
  "has_member_keyword_color" :hash 3927539163)
 bool (member-keyword string))

(defgmethod
 (code-highlighter+get-member-keyword-color :class 'code-highlighter :bind
  "get_member_keyword_color" :hash 3855908743)
 color (member-keyword string))

(defgmethod
 (code-highlighter+set-member-keyword-colors :class 'code-highlighter :bind
  "set_member_keyword_colors" :hash 4155329257)
 :void (member-keyword dictionary))

(defgmethod
 (code-highlighter+clear-member-keyword-colors :class 'code-highlighter :bind
  "clear_member_keyword_colors" :hash 3218959716)
 :void)

(defgmethod
 (code-highlighter+get-member-keyword-colors :class 'code-highlighter :bind
  "get_member_keyword_colors" :hash 3102165223)
 dictionary)

(defgmethod
 (code-highlighter+add-color-region :class 'code-highlighter :bind
  "add_color_region" :hash 2924977451)
 :void (start-key string) (end-key string) (color color) (line-only bool))

(defgmethod
 (code-highlighter+remove-color-region :class 'code-highlighter :bind
  "remove_color_region" :hash 83702148)
 :void (start-key string))

(defgmethod
 (code-highlighter+has-color-region :class 'code-highlighter :bind
  "has_color_region" :hash 3927539163)
 bool (start-key string))

(defgmethod
 (code-highlighter+set-color-regions :class 'code-highlighter :bind
  "set_color_regions" :hash 4155329257)
 :void (color-regions dictionary))

(defgmethod
 (code-highlighter+clear-color-regions :class 'code-highlighter :bind
  "clear_color_regions" :hash 3218959716)
 :void)

(defgmethod
 (code-highlighter+get-color-regions :class 'code-highlighter :bind
  "get_color_regions" :hash 3102165223)
 dictionary)

(defgmethod
 (code-highlighter+set-function-color :class 'code-highlighter :bind
  "set_function_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (code-highlighter+get-function-color :class 'code-highlighter :bind
  "get_function_color" :hash 3444240500)
 color)

(defgmethod
 (code-highlighter+set-number-color :class 'code-highlighter :bind
  "set_number_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (code-highlighter+get-number-color :class 'code-highlighter :bind
  "get_number_color" :hash 3444240500)
 color)

(defgmethod
 (code-highlighter+set-symbol-color :class 'code-highlighter :bind
  "set_symbol_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (code-highlighter+get-symbol-color :class 'code-highlighter :bind
  "get_symbol_color" :hash 3444240500)
 color)

(defgmethod
 (code-highlighter+set-member-variable-color :class 'code-highlighter :bind
  "set_member_variable_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (code-highlighter+get-member-variable-color :class 'code-highlighter :bind
  "get_member_variable_color" :hash 3444240500)
 color)