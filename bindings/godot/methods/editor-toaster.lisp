(common-lisp:in-package :%godot)


(defgmethod
 (editor-toaster+push-toast :class 'editor-toaster :bind "push_toast" :hash
  1813923476)
 :void (message string) (severity editor-toaster+severity) (tooltip string))