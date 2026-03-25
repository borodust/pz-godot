(common-lisp:in-package :%godot)


(defgmethod
 (button-group+get-pressed-button :class 'button-group :bind
  "get_pressed_button" :hash 3886434893)
 base-button)

(defgmethod
 (button-group+get-buttons :class 'button-group :bind "get_buttons" :hash
  2915620761)
 array)

(defgmethod
 (button-group+set-allow-unpress :class 'button-group :bind "set_allow_unpress"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (button-group+is-allow-unpress :class 'button-group :bind "is_allow_unpress"
  :hash 2240911060)
 bool)