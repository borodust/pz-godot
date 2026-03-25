(common-lisp:in-package :%godot)


(defgmethod
 (open-xrhaptic-vibration+set-duration :class 'open-xrhaptic-vibration :bind
  "set_duration" :hash 1286410249)
 :void (duration int))

(defgmethod
 (open-xrhaptic-vibration+get-duration :class 'open-xrhaptic-vibration :bind
  "get_duration" :hash 3905245786)
 int)

(defgmethod
 (open-xrhaptic-vibration+set-frequency :class 'open-xrhaptic-vibration :bind
  "set_frequency" :hash 373806689)
 :void (frequency float))

(defgmethod
 (open-xrhaptic-vibration+get-frequency :class 'open-xrhaptic-vibration :bind
  "get_frequency" :hash 1740695150)
 float)

(defgmethod
 (open-xrhaptic-vibration+set-amplitude :class 'open-xrhaptic-vibration :bind
  "set_amplitude" :hash 373806689)
 :void (amplitude float))

(defgmethod
 (open-xrhaptic-vibration+get-amplitude :class 'open-xrhaptic-vibration :bind
  "get_amplitude" :hash 1740695150)
 float)