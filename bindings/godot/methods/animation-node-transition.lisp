(common-lisp:in-package :%godot)


(defgmethod
 (animation-node-transition+set-input-count :class 'animation-node-transition
  :bind "set_input_count" :hash 1286410249)
 :void (input-count int))

(defgmethod
 (animation-node-transition+set-input-as-auto-advance :class
  'animation-node-transition :bind "set_input_as_auto_advance" :hash 300928843)
 :void (input int) (enable bool))

(defgmethod
 (animation-node-transition+is-input-set-as-auto-advance :class
  'animation-node-transition :bind "is_input_set_as_auto_advance" :hash
  1116898809)
 bool (input int))

(defgmethod
 (animation-node-transition+set-input-break-loop-at-end :class
  'animation-node-transition :bind "set_input_break_loop_at_end" :hash
  300928843)
 :void (input int) (enable bool))

(defgmethod
 (animation-node-transition+is-input-loop-broken-at-end :class
  'animation-node-transition :bind "is_input_loop_broken_at_end" :hash
  1116898809)
 bool (input int))

(defgmethod
 (animation-node-transition+set-input-reset :class 'animation-node-transition
  :bind "set_input_reset" :hash 300928843)
 :void (input int) (enable bool))

(defgmethod
 (animation-node-transition+is-input-reset :class 'animation-node-transition
  :bind "is_input_reset" :hash 1116898809)
 bool (input int))

(defgmethod
 (animation-node-transition+set-xfade-time :class 'animation-node-transition
  :bind "set_xfade_time" :hash 373806689)
 :void (time float))

(defgmethod
 (animation-node-transition+get-xfade-time :class 'animation-node-transition
  :bind "get_xfade_time" :hash 1740695150)
 float)

(defgmethod
 (animation-node-transition+set-xfade-curve :class 'animation-node-transition
  :bind "set_xfade_curve" :hash 270443179)
 :void (curve curve))

(defgmethod
 (animation-node-transition+get-xfade-curve :class 'animation-node-transition
  :bind "get_xfade_curve" :hash 2460114913)
 curve)

(defgmethod
 (animation-node-transition+set-allow-transition-to-self :class
  'animation-node-transition :bind "set_allow_transition_to_self" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (animation-node-transition+is-allow-transition-to-self :class
  'animation-node-transition :bind "is_allow_transition_to_self" :hash
  36873697)
 bool)