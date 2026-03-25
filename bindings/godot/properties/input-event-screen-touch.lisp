(common-lisp:in-package :%godot)


(defgproperty input-event-screen-touch+index 'input-event-screen-touch :get
 'input-event-screen-touch+get-index :set 'input-event-screen-touch+set-index)

(defgproperty input-event-screen-touch+position 'input-event-screen-touch :get
 'input-event-screen-touch+get-position :set
 'input-event-screen-touch+set-position)

(defgproperty input-event-screen-touch+canceled 'input-event-screen-touch :set
 'input-event-screen-touch+set-canceled)

(defgproperty input-event-screen-touch+pressed 'input-event-screen-touch :set
 'input-event-screen-touch+set-pressed)

(defgproperty input-event-screen-touch+double-tap 'input-event-screen-touch
 :get 'input-event-screen-touch+is-double-tap :set
 'input-event-screen-touch+set-double-tap)