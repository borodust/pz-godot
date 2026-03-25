(common-lisp:in-package :%godot)


(defgmethod
 (confirmation-dialog+get-cancel-button :class 'confirmation-dialog :bind
  "get_cancel_button" :hash 1856205918)
 button)

(defgmethod
 (confirmation-dialog+set-cancel-button-text :class 'confirmation-dialog :bind
  "set_cancel_button_text" :hash 83702148)
 :void (text string))

(defgmethod
 (confirmation-dialog+get-cancel-button-text :class 'confirmation-dialog :bind
  "get_cancel_button_text" :hash 201670096)
 string)