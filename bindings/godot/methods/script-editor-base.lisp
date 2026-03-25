(common-lisp:in-package :%godot)


(defgmethod
 (script-editor-base+get-base-editor :class 'script-editor-base :bind
  "get_base_editor" :hash 2783021301)
 control)

(defgmethod
 (script-editor-base+add-syntax-highlighter :class 'script-editor-base :bind
  "add_syntax_highlighter" :hash 1092774468)
 :void (highlighter editor-syntax-highlighter))