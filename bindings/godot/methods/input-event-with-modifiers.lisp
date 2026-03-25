(common-lisp:in-package :%godot)


(defgmethod
 (input-event-with-modifiers+set-command-or-control-autoremap :class
  'input-event-with-modifiers :bind "set_command_or_control_autoremap" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (input-event-with-modifiers+is-command-or-control-autoremap :class
  'input-event-with-modifiers :bind "is_command_or_control_autoremap" :hash
  36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+is-command-or-control-pressed :class
  'input-event-with-modifiers :bind "is_command_or_control_pressed" :hash
  36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+set-alt-pressed :class 'input-event-with-modifiers
  :bind "set_alt_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-with-modifiers+is-alt-pressed :class 'input-event-with-modifiers
  :bind "is_alt_pressed" :hash 36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+set-shift-pressed :class
  'input-event-with-modifiers :bind "set_shift_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-with-modifiers+is-shift-pressed :class
  'input-event-with-modifiers :bind "is_shift_pressed" :hash 36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+set-ctrl-pressed :class
  'input-event-with-modifiers :bind "set_ctrl_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-with-modifiers+is-ctrl-pressed :class 'input-event-with-modifiers
  :bind "is_ctrl_pressed" :hash 36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+set-meta-pressed :class
  'input-event-with-modifiers :bind "set_meta_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-with-modifiers+is-meta-pressed :class 'input-event-with-modifiers
  :bind "is_meta_pressed" :hash 36873697)
 bool)

(defgmethod
 (input-event-with-modifiers+get-modifiers-mask :class
  'input-event-with-modifiers :bind "get_modifiers_mask" :hash 1258259499)
 key-modifier-mask)