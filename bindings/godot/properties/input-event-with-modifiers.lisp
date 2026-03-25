(common-lisp:in-package :%godot)


(defgproperty input-event-with-modifiers+command-or-control-autoremap
 'input-event-with-modifiers :get
 'input-event-with-modifiers+is-command-or-control-autoremap :set
 'input-event-with-modifiers+set-command-or-control-autoremap)

(defgproperty input-event-with-modifiers+alt-pressed
 'input-event-with-modifiers :get 'input-event-with-modifiers+is-alt-pressed
 :set 'input-event-with-modifiers+set-alt-pressed)

(defgproperty input-event-with-modifiers+shift-pressed
 'input-event-with-modifiers :get 'input-event-with-modifiers+is-shift-pressed
 :set 'input-event-with-modifiers+set-shift-pressed)

(defgproperty input-event-with-modifiers+ctrl-pressed
 'input-event-with-modifiers :get 'input-event-with-modifiers+is-ctrl-pressed
 :set 'input-event-with-modifiers+set-ctrl-pressed)

(defgproperty input-event-with-modifiers+meta-pressed
 'input-event-with-modifiers :get 'input-event-with-modifiers+is-meta-pressed
 :set 'input-event-with-modifiers+set-meta-pressed)