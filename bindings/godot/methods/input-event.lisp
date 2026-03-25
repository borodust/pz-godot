(common-lisp:in-package :%godot)


(defgmethod
 (input-event+set-device :class 'input-event :bind "set_device" :hash
  1286410249)
 :void (device int))

(defgmethod
 (input-event+get-device :class 'input-event :bind "get_device" :hash
  3905245786)
 int)

(defgmethod
 (input-event+is-action :class 'input-event :bind "is_action" :hash 1558498928)
 bool (action string-name) (exact-match bool))

(defgmethod
 (input-event+is-action-pressed :class 'input-event :bind "is_action_pressed"
  :hash 1631499404)
 bool (action string-name) (allow-echo bool) (exact-match bool))

(defgmethod
 (input-event+is-action-released :class 'input-event :bind "is_action_released"
  :hash 1558498928)
 bool (action string-name) (exact-match bool))

(defgmethod
 (input-event+get-action-strength :class 'input-event :bind
  "get_action_strength" :hash 801543509)
 float (action string-name) (exact-match bool))

(defgmethod
 (input-event+is-canceled :class 'input-event :bind "is_canceled" :hash
  36873697)
 bool)

(defgmethod
 (input-event+is-pressed :class 'input-event :bind "is_pressed" :hash 36873697)
 bool)

(defgmethod
 (input-event+is-released :class 'input-event :bind "is_released" :hash
  36873697)
 bool)

(defgmethod
 (input-event+is-echo :class 'input-event :bind "is_echo" :hash 36873697) bool)

(defgmethod
 (input-event+as-text :class 'input-event :bind "as_text" :hash 201670096)
 string)

(defgmethod
 (input-event+is-match :class 'input-event :bind "is_match" :hash 1754951977)
 bool (event input-event) (exact-match bool))

(defgmethod
 (input-event+is-action-type :class 'input-event :bind "is_action_type" :hash
  36873697)
 bool)

(defgmethod
 (input-event+accumulate :class 'input-event :bind "accumulate" :hash
  1062211774)
 bool (with-event input-event))

(defgmethod
 (input-event+xformed-by :class 'input-event :bind "xformed_by" :hash
  1282766827)
 input-event (xform transform-2d) (local-ofs vector-2))