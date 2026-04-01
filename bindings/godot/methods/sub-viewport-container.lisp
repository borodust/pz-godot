(common-lisp:in-package :%godot)


(defgmethod
 (sub-viewport-container+%propagate-input-event :class 'sub-viewport-container
  :bind "_propagate_input_event" :hash 3738334489 :virtual common-lisp:t)
 bool (event input-event))

(defgmethod
 (sub-viewport-container+set-stretch :class 'sub-viewport-container :bind
  "set_stretch" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (sub-viewport-container+is-stretch-enabled :class 'sub-viewport-container
  :bind "is_stretch_enabled" :hash 36873697)
 bool)

(defgmethod
 (sub-viewport-container+set-stretch-shrink :class 'sub-viewport-container
  :bind "set_stretch_shrink" :hash 1286410249)
 :void (amount int))

(defgmethod
 (sub-viewport-container+get-stretch-shrink :class 'sub-viewport-container
  :bind "get_stretch_shrink" :hash 3905245786)
 int)

(defgmethod
 (sub-viewport-container+set-mouse-target :class 'sub-viewport-container :bind
  "set_mouse_target" :hash 2586408642)
 :void (amount bool))

(defgmethod
 (sub-viewport-container+is-mouse-target-enabled :class 'sub-viewport-container
  :bind "is_mouse_target_enabled" :hash 2240911060)
 bool)