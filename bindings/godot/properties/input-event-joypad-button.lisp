(common-lisp:in-package :%godot)


(defgproperty input-event-joypad-button+button-index 'input-event-joypad-button
 :get 'input-event-joypad-button+get-button-index :set
 'input-event-joypad-button+set-button-index)

(defgproperty input-event-joypad-button+pressure 'input-event-joypad-button
 :get 'input-event-joypad-button+get-pressure :set
 'input-event-joypad-button+set-pressure)

(defgproperty input-event-joypad-button+pressed 'input-event-joypad-button :set
 'input-event-joypad-button+set-pressed)