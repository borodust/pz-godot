(common-lisp:in-package :%godot)


(defgmethod
 (input-event-action+set-action :class 'input-event-action :bind "set_action"
  :hash 3304788590)
 :void (action string-name))

(defgmethod
 (input-event-action+get-action :class 'input-event-action :bind "get_action"
  :hash 2002593661)
 string-name)

(defgmethod
 (input-event-action+set-pressed :class 'input-event-action :bind "set_pressed"
  :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-action+set-strength :class 'input-event-action :bind
  "set_strength" :hash 373806689)
 :void (strength float))

(defgmethod
 (input-event-action+get-strength :class 'input-event-action :bind
  "get_strength" :hash 1740695150)
 float)

(defgmethod
 (input-event-action+set-event-index :class 'input-event-action :bind
  "set_event_index" :hash 1286410249)
 :void (index int))

(defgmethod
 (input-event-action+get-event-index :class 'input-event-action :bind
  "get_event_index" :hash 3905245786)
 int)