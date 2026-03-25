(common-lisp:in-package :%godot)


(defgmethod
 (input-event-mouse-motion+set-tilt :class 'input-event-mouse-motion :bind
  "set_tilt" :hash 743155724)
 :void (tilt vector-2))

(defgmethod
 (input-event-mouse-motion+get-tilt :class 'input-event-mouse-motion :bind
  "get_tilt" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-mouse-motion+set-pressure :class 'input-event-mouse-motion :bind
  "set_pressure" :hash 373806689)
 :void (pressure float))

(defgmethod
 (input-event-mouse-motion+get-pressure :class 'input-event-mouse-motion :bind
  "get_pressure" :hash 1740695150)
 float)

(defgmethod
 (input-event-mouse-motion+set-pen-inverted :class 'input-event-mouse-motion
  :bind "set_pen_inverted" :hash 2586408642)
 :void (pen-inverted bool))

(defgmethod
 (input-event-mouse-motion+get-pen-inverted :class 'input-event-mouse-motion
  :bind "get_pen_inverted" :hash 36873697)
 bool)

(defgmethod
 (input-event-mouse-motion+set-relative :class 'input-event-mouse-motion :bind
  "set_relative" :hash 743155724)
 :void (relative vector-2))

(defgmethod
 (input-event-mouse-motion+get-relative :class 'input-event-mouse-motion :bind
  "get_relative" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-mouse-motion+set-screen-relative :class 'input-event-mouse-motion
  :bind "set_screen_relative" :hash 743155724)
 :void (relative vector-2))

(defgmethod
 (input-event-mouse-motion+get-screen-relative :class 'input-event-mouse-motion
  :bind "get_screen_relative" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-mouse-motion+set-velocity :class 'input-event-mouse-motion :bind
  "set_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (input-event-mouse-motion+get-velocity :class 'input-event-mouse-motion :bind
  "get_velocity" :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-mouse-motion+set-screen-velocity :class 'input-event-mouse-motion
  :bind "set_screen_velocity" :hash 743155724)
 :void (velocity vector-2))

(defgmethod
 (input-event-mouse-motion+get-screen-velocity :class 'input-event-mouse-motion
  :bind "get_screen_velocity" :hash 3341600327)
 vector-2)