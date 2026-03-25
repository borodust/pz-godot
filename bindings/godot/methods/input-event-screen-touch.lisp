(common-lisp:in-package :%godot)


(defgmethod
 (input-event-screen-touch+set-index :class 'input-event-screen-touch :bind
  "set_index" :hash 1286410249)
 :void (index int))

(defgmethod
 (input-event-screen-touch+get-index :class 'input-event-screen-touch :bind
  "get_index" :hash 3905245786)
 int)

(defgmethod
 (input-event-screen-touch+set-position :class 'input-event-screen-touch :bind
  "set_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (input-event-screen-touch+get-position :class 'input-event-screen-touch :bind
  "get_position" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-touch+set-pressed :class 'input-event-screen-touch :bind
  "set_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-screen-touch+set-canceled :class 'input-event-screen-touch :bind
  "set_canceled" :hash 2586408642)
 :void (canceled bool))

(defgmethod
 (input-event-screen-touch+set-double-tap :class 'input-event-screen-touch
  :bind "set_double_tap" :hash 2586408642)
 :void (double-tap bool))

(defgmethod
 (input-event-screen-touch+is-double-tap :class 'input-event-screen-touch :bind
  "is_double_tap" :hash 36873697)
 bool)