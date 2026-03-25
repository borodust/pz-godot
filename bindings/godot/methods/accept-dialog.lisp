(common-lisp:in-package :%godot)


(defgmethod
 (accept-dialog+get-ok-button :class 'accept-dialog :bind "get_ok_button" :hash
  1856205918)
 button)

(defgmethod
 (accept-dialog+get-label :class 'accept-dialog :bind "get_label" :hash
  566733104)
 label)

(defgmethod
 (accept-dialog+set-hide-on-ok :class 'accept-dialog :bind "set_hide_on_ok"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (accept-dialog+get-hide-on-ok :class 'accept-dialog :bind "get_hide_on_ok"
  :hash 36873697)
 bool)

(defgmethod
 (accept-dialog+set-close-on-escape :class 'accept-dialog :bind
  "set_close_on_escape" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (accept-dialog+get-close-on-escape :class 'accept-dialog :bind
  "get_close_on_escape" :hash 36873697)
 bool)

(defgmethod
 (accept-dialog+add-button :class 'accept-dialog :bind "add_button" :hash
  3328440682)
 button (text string) (right bool) (action string))

(defgmethod
 (accept-dialog+add-cancel-button :class 'accept-dialog :bind
  "add_cancel_button" :hash 242045556)
 button (name string))

(defgmethod
 (accept-dialog+remove-button :class 'accept-dialog :bind "remove_button" :hash
  2068354942)
 :void (button button))

(defgmethod
 (accept-dialog+register-text-enter :class 'accept-dialog :bind
  "register_text_enter" :hash 3714008017)
 :void (line-edit line-edit))

(defgmethod
 (accept-dialog+set-text :class 'accept-dialog :bind "set_text" :hash 83702148)
 :void (text string))

(defgmethod
 (accept-dialog+get-text :class 'accept-dialog :bind "get_text" :hash
  201670096)
 string)

(defgmethod
 (accept-dialog+set-autowrap :class 'accept-dialog :bind "set_autowrap" :hash
  2586408642)
 :void (autowrap bool))

(defgmethod
 (accept-dialog+has-autowrap :class 'accept-dialog :bind "has_autowrap" :hash
  2240911060)
 bool)

(defgmethod
 (accept-dialog+set-ok-button-text :class 'accept-dialog :bind
  "set_ok_button_text" :hash 83702148)
 :void (text string))

(defgmethod
 (accept-dialog+get-ok-button-text :class 'accept-dialog :bind
  "get_ok_button_text" :hash 201670096)
 string)