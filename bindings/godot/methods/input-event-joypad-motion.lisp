(common-lisp:in-package :%godot)


(defgmethod
 (input-event-joypad-motion+set-axis :class 'input-event-joypad-motion :bind
  "set_axis" :hash 1332685170)
 :void (axis joy-axis))

(defgmethod
 (input-event-joypad-motion+get-axis :class 'input-event-joypad-motion :bind
  "get_axis" :hash 4019121683)
 joy-axis)

(defgmethod
 (input-event-joypad-motion+set-axis-value :class 'input-event-joypad-motion
  :bind "set_axis_value" :hash 373806689)
 :void (axis-value float))

(defgmethod
 (input-event-joypad-motion+get-axis-value :class 'input-event-joypad-motion
  :bind "get_axis_value" :hash 1740695150)
 float)