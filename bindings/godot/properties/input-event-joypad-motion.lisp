(common-lisp:in-package :%godot)


(defgproperty input-event-joypad-motion+axis 'input-event-joypad-motion :get
 'input-event-joypad-motion+get-axis :set 'input-event-joypad-motion+set-axis)

(defgproperty input-event-joypad-motion+axis-value 'input-event-joypad-motion
 :get 'input-event-joypad-motion+get-axis-value :set
 'input-event-joypad-motion+set-axis-value)