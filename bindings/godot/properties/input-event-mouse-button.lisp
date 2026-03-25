(common-lisp:in-package :%godot)


(defgproperty input-event-mouse-button+factor 'input-event-mouse-button :get
 'input-event-mouse-button+get-factor :set 'input-event-mouse-button+set-factor)

(defgproperty input-event-mouse-button+button-index 'input-event-mouse-button
 :get 'input-event-mouse-button+get-button-index :set
 'input-event-mouse-button+set-button-index)

(defgproperty input-event-mouse-button+canceled 'input-event-mouse-button :set
 'input-event-mouse-button+set-canceled)

(defgproperty input-event-mouse-button+pressed 'input-event-mouse-button :set
 'input-event-mouse-button+set-pressed)

(defgproperty input-event-mouse-button+double-click 'input-event-mouse-button
 :get 'input-event-mouse-button+is-double-click :set
 'input-event-mouse-button+set-double-click)