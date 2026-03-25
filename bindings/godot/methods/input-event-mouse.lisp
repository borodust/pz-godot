(common-lisp:in-package :%godot)


(defgmethod
 (input-event-mouse+set-button-mask :class 'input-event-mouse :bind
  "set_button_mask" :hash 3950145251)
 :void (button-mask mouse-button-mask))

(defgmethod
 (input-event-mouse+get-button-mask :class 'input-event-mouse :bind
  "get_button_mask" :hash 2512161324)
 mouse-button-mask)

(defgmethod
 (input-event-mouse+set-position :class 'input-event-mouse :bind "set_position"
  :hash 743155724)
 :void (position vector-2))

(defgmethod
 (input-event-mouse+get-position :class 'input-event-mouse :bind "get_position"
  :hash 3341600327)
 vector-2)

(defgmethod
 (input-event-mouse+set-global-position :class 'input-event-mouse :bind
  "set_global_position" :hash 743155724)
 :void (global-position vector-2))

(defgmethod
 (input-event-mouse+get-global-position :class 'input-event-mouse :bind
  "get_global_position" :hash 3341600327)
 vector-2)