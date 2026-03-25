(common-lisp:in-package :%godot)


(defgmethod
 (input-event-joypad-button+set-button-index :class 'input-event-joypad-button
  :bind "set_button_index" :hash 1466368136)
 :void (button-index joy-button))

(defgmethod
 (input-event-joypad-button+get-button-index :class 'input-event-joypad-button
  :bind "get_button_index" :hash 595588182)
 joy-button)

(defgmethod
 (input-event-joypad-button+set-pressure :class 'input-event-joypad-button
  :bind "set_pressure" :hash 373806689)
 :void (pressure float))

(defgmethod
 (input-event-joypad-button+get-pressure :class 'input-event-joypad-button
  :bind "get_pressure" :hash 1740695150)
 float)

(defgmethod
 (input-event-joypad-button+set-pressed :class 'input-event-joypad-button :bind
  "set_pressed" :hash 2586408642)
 :void (pressed bool))