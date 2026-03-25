(common-lisp:in-package :%godot)


(defgmethod
 (input-event-mouse-button+set-factor :class 'input-event-mouse-button :bind
  "set_factor" :hash 373806689)
 :void (factor float))

(defgmethod
 (input-event-mouse-button+get-factor :class 'input-event-mouse-button :bind
  "get_factor" :hash 1740695150)
 float)

(defgmethod
 (input-event-mouse-button+set-button-index :class 'input-event-mouse-button
  :bind "set_button_index" :hash 3624991109)
 :void (button-index mouse-button))

(defgmethod
 (input-event-mouse-button+get-button-index :class 'input-event-mouse-button
  :bind "get_button_index" :hash 1132662608)
 mouse-button)

(defgmethod
 (input-event-mouse-button+set-pressed :class 'input-event-mouse-button :bind
  "set_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-mouse-button+set-canceled :class 'input-event-mouse-button :bind
  "set_canceled" :hash 2586408642)
 :void (canceled bool))

(defgmethod
 (input-event-mouse-button+set-double-click :class 'input-event-mouse-button
  :bind "set_double_click" :hash 2586408642)
 :void (double-click bool))

(defgmethod
 (input-event-mouse-button+is-double-click :class 'input-event-mouse-button
  :bind "is_double_click" :hash 36873697)
 bool)