(common-lisp:in-package :%godot)


(defgmethod
 (input-event-from-window+set-window-id :class 'input-event-from-window :bind
  "set_window_id" :hash 1286410249)
 :void (id int))

(defgmethod
 (input-event-from-window+get-window-id :class 'input-event-from-window :bind
  "get_window_id" :hash 3905245786)
 int)