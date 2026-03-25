(common-lisp:in-package :%godot)


(defgmethod
 (input-event-midi+set-channel :class 'input-event-midi :bind "set_channel"
  :hash 1286410249)
 :void (channel int))

(defgmethod
 (input-event-midi+get-channel :class 'input-event-midi :bind "get_channel"
  :hash 3905245786)
 int)

(defgmethod
 (input-event-midi+set-message :class 'input-event-midi :bind "set_message"
  :hash 1064271510)
 :void (message midimessage))

(defgmethod
 (input-event-midi+get-message :class 'input-event-midi :bind "get_message"
  :hash 1936512097)
 midimessage)

(defgmethod
 (input-event-midi+set-pitch :class 'input-event-midi :bind "set_pitch" :hash
  1286410249)
 :void (pitch int))

(defgmethod
 (input-event-midi+get-pitch :class 'input-event-midi :bind "get_pitch" :hash
  3905245786)
 int)

(defgmethod
 (input-event-midi+set-velocity :class 'input-event-midi :bind "set_velocity"
  :hash 1286410249)
 :void (velocity int))

(defgmethod
 (input-event-midi+get-velocity :class 'input-event-midi :bind "get_velocity"
  :hash 3905245786)
 int)

(defgmethod
 (input-event-midi+set-instrument :class 'input-event-midi :bind
  "set_instrument" :hash 1286410249)
 :void (instrument int))

(defgmethod
 (input-event-midi+get-instrument :class 'input-event-midi :bind
  "get_instrument" :hash 3905245786)
 int)

(defgmethod
 (input-event-midi+set-pressure :class 'input-event-midi :bind "set_pressure"
  :hash 1286410249)
 :void (pressure int))

(defgmethod
 (input-event-midi+get-pressure :class 'input-event-midi :bind "get_pressure"
  :hash 3905245786)
 int)

(defgmethod
 (input-event-midi+set-controller-number :class 'input-event-midi :bind
  "set_controller_number" :hash 1286410249)
 :void (controller-number int))

(defgmethod
 (input-event-midi+get-controller-number :class 'input-event-midi :bind
  "get_controller_number" :hash 3905245786)
 int)

(defgmethod
 (input-event-midi+set-controller-value :class 'input-event-midi :bind
  "set_controller_value" :hash 1286410249)
 :void (controller-value int))

(defgmethod
 (input-event-midi+get-controller-value :class 'input-event-midi :bind
  "get_controller_value" :hash 3905245786)
 int)