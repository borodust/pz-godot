(common-lisp:in-package :%godot)


(defgproperty input-event-key+pressed 'input-event-key :set
 'input-event-key+set-pressed)

(defgproperty input-event-key+keycode 'input-event-key :get
 'input-event-key+get-keycode :set 'input-event-key+set-keycode)

(defgproperty input-event-key+physical-keycode 'input-event-key :get
 'input-event-key+get-physical-keycode :set
 'input-event-key+set-physical-keycode)

(defgproperty input-event-key+key-label 'input-event-key :get
 'input-event-key+get-key-label :set 'input-event-key+set-key-label)

(defgproperty input-event-key+unicode 'input-event-key :get
 'input-event-key+get-unicode :set 'input-event-key+set-unicode)

(defgproperty input-event-key+location 'input-event-key :get
 'input-event-key+get-location :set 'input-event-key+set-location)

(defgproperty input-event-key+echo 'input-event-key :set
 'input-event-key+set-echo)