(common-lisp:in-package :%godot)


(defgmethod
 (input-event-screen-drag+set-index :class 'input-event-screen-drag :bind
  "set_index" :hash 1286410249)
 :void (index int))

(defgmethod
 (input-event-screen-drag+get-index :class 'input-event-screen-drag :bind
  "get_index" :hash 3905245786)
 int)

(defgmethod
 (input-event-screen-drag+set-tilt :class 'input-event-screen-drag :bind
  "set_tilt" :hash 743155724)
 :void (tilt vector-2))

(defgmethod
 (input-event-screen-drag+get-tilt :class 'input-event-screen-drag :bind
  "get_tilt" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-drag+set-pressure :class 'input-event-screen-drag :bind
  "set_pressure" :hash 373806689)
 :void (pressure float))

(defgmethod
 (input-event-screen-drag+get-pressure :class 'input-event-screen-drag :bind
  "get_pressure" :hash 1740695150)
 float)

(defgmethod
 (input-event-screen-drag+set-pen-inverted :class 'input-event-screen-drag
  :bind "set_pen_inverted" :hash 2586408642)
 :void (pen-inverted bool))

(defgmethod
 (input-event-screen-drag+get-pen-inverted :class 'input-event-screen-drag
  :bind "get_pen_inverted" :hash 36873697)
 bool)

(defgmethod
 (input-event-screen-drag+set-position :class 'input-event-screen-drag :bind
  "set_position" :hash 743155724)
 :void (position vector-2))

(defgmethod
 (input-event-screen-drag+get-position :class 'input-event-screen-drag :bind
  "get_position" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-drag+set-relative :class 'input-event-screen-drag :bind
  "set_relative" :hash 743155724)
 :void (relative vector-2))

(defgmethod
 (input-event-screen-drag+get-relative :class 'input-event-screen-drag :bind
  "get_relative" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-drag+set-screen-relative :class 'input-event-screen-drag
  :bind "set_screen_relative" :hash 743155724)
 :void (relative vector-2))

(defgmethod
 (input-event-screen-drag+get-screen-relative :class 'input-event-screen-drag
  :bind "get_screen_relative" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-drag+set-velocity :class 'input-event-screen-drag :bind
  "set_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (input-event-screen-drag+get-velocity :class 'input-event-screen-drag :bind
  "get_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-screen-drag+set-screen-velocity :class 'input-event-screen-drag
  :bind "set_screen_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (input-event-screen-drag+get-screen-velocity :class 'input-event-screen-drag
  :bind "get_screen_velocity" :hash 3341600327)
 vector-2)