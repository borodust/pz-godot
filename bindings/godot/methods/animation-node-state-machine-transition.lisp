(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-state-machine-transition+set-switch-mode :class
  'animation-node-state-machine-transition :bind "set_switch_mode" :hash
  2074906633)
 :void (mode animation-node-state-machine-transition+switch-mode))

(defgmethod
 (animation-node-state-machine-transition+get-switch-mode :class
  'animation-node-state-machine-transition :bind "get_switch_mode" :hash
  2138562085)
 animation-node-state-machine-transition+switch-mode)

(defgmethod
 (animation-node-state-machine-transition+set-advance-mode :class
  'animation-node-state-machine-transition :bind "set_advance_mode" :hash
  1210869868)
 :void (mode animation-node-state-machine-transition+advance-mode))

(defgmethod
 (animation-node-state-machine-transition+get-advance-mode :class
  'animation-node-state-machine-transition :bind "get_advance_mode" :hash
  61101689)
 animation-node-state-machine-transition+advance-mode)

(defgmethod
 (animation-node-state-machine-transition+set-advance-condition :class
  'animation-node-state-machine-transition :bind "set_advance_condition" :hash
  3304788590)
 :void (name string-name))

(defgmethod
 (animation-node-state-machine-transition+get-advance-condition :class
  'animation-node-state-machine-transition :bind "get_advance_condition" :hash
  2002593661)
 string-name)

(defgmethod
 (animation-node-state-machine-transition+set-xfade-time :class
  'animation-node-state-machine-transition :bind "set_xfade_time" :hash
  373806689)
 :void (secs float))

(defgmethod
 (animation-node-state-machine-transition+get-xfade-time :class
  'animation-node-state-machine-transition :bind "get_xfade_time" :hash
  1740695150)
 float)

(defgmethod
 (animation-node-state-machine-transition+set-xfade-curve :class
  'animation-node-state-machine-transition :bind "set_xfade_curve" :hash
  270443179)
 :void (curve curve))

(defgmethod
 (animation-node-state-machine-transition+get-xfade-curve :class
  'animation-node-state-machine-transition :bind "get_xfade_curve" :hash
  2460114913)
 curve)

(defgmethod
 (animation-node-state-machine-transition+set-break-loop-at-end :class
  'animation-node-state-machine-transition :bind "set_break_loop_at_end" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-state-machine-transition+is-loop-broken-at-end :class
  'animation-node-state-machine-transition :bind "is_loop_broken_at_end" :hash
  36873697)
 bool)

(defgmethod
 (animation-node-state-machine-transition+set-reset :class
  'animation-node-state-machine-transition :bind "set_reset" :hash 2586408642)
 :void (reset bool))

(defgmethod
 (animation-node-state-machine-transition+is-reset :class
  'animation-node-state-machine-transition :bind "is_reset" :hash 36873697)
 bool)

(defgmethod
 (animation-node-state-machine-transition+set-priority :class
  'animation-node-state-machine-transition :bind "set_priority" :hash
  1286410249)
 :void (priority int))

(defgmethod
 (animation-node-state-machine-transition+get-priority :class
  'animation-node-state-machine-transition :bind "get_priority" :hash
  3905245786)
 int)

(defgmethod
 (animation-node-state-machine-transition+set-advance-expression :class
  'animation-node-state-machine-transition :bind "set_advance_expression" :hash
  83702148)
 :void (text string))

(defgmethod
 (animation-node-state-machine-transition+get-advance-expression :class
  'animation-node-state-machine-transition :bind "get_advance_expression" :hash
  201670096)
 string)