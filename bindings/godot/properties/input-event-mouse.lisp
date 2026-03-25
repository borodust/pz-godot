(common-lisp:in-package :%godot)


(defgproperty input-event-mouse+button-mask 'input-event-mouse :get
 'input-event-mouse+get-button-mask :set 'input-event-mouse+set-button-mask)

(defgproperty input-event-mouse+position 'input-event-mouse :get
 'input-event-mouse+get-position :set 'input-event-mouse+set-position)

(defgproperty input-event-mouse+global-position 'input-event-mouse :get
 'input-event-mouse+get-global-position :set
 'input-event-mouse+set-global-position)