(common-lisp:in-package :%godot)


(defgmethod
 (editor-command-palette+add-command :class 'editor-command-palette :bind
  "add_command" :hash 864043298)
 :void (command-name string) (key-name string) (binded-callable callable)
 (shortcut-text string))

(defgmethod
 (editor-command-palette+remove-command :class 'editor-command-palette :bind
  "remove_command" :hash 83702148)
 :void (key-name string))