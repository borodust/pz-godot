(common-lisp:in-package :%godot)


(defgmethod
 (input-event-key+set-pressed :class 'input-event-key :bind "set_pressed" :hash
  2586408642)
 :void (pressed bool))

(defgmethod
 (input-event-key+set-keycode :class 'input-event-key :bind "set_keycode" :hash
  888074362)
 :void (keycode key))

(defgmethod
 (input-event-key+get-keycode :class 'input-event-key :bind "get_keycode" :hash
  1585896689)
 key)

(defgmethod
 (input-event-key+set-physical-keycode :class 'input-event-key :bind
  "set_physical_keycode" :hash 888074362)
 :void (physical-keycode key))

(defgmethod
 (input-event-key+get-physical-keycode :class 'input-event-key :bind
  "get_physical_keycode" :hash 1585896689)
 key)

(defgmethod
 (input-event-key+set-key-label :class 'input-event-key :bind "set_key_label"
  :hash 888074362)
 :void (key-label key))

(defgmethod
 (input-event-key+get-key-label :class 'input-event-key :bind "get_key_label"
  :hash 1585896689)
 key)

(defgmethod
 (input-event-key+set-unicode :class 'input-event-key :bind "set_unicode" :hash
  1286410249)
 :void (unicode int))

(defgmethod
 (input-event-key+get-unicode :class 'input-event-key :bind "get_unicode" :hash
  3905245786)
 int)

(defgmethod
 (input-event-key+set-location :class 'input-event-key :bind "set_location"
  :hash 634453155)
 :void (location key-location))

(defgmethod
 (input-event-key+get-location :class 'input-event-key :bind "get_location"
  :hash 211810873)
 key-location)

(defgmethod
 (input-event-key+set-echo :class 'input-event-key :bind "set_echo" :hash
  2586408642)
 :void (echo bool))

(defgmethod
 (input-event-key+get-keycode-with-modifiers :class 'input-event-key :bind
  "get_keycode_with_modifiers" :hash 1585896689)
 key)

(defgmethod
 (input-event-key+get-physical-keycode-with-modifiers :class 'input-event-key
  :bind "get_physical_keycode_with_modifiers" :hash 1585896689)
 key)

(defgmethod
 (input-event-key+get-key-label-with-modifiers :class 'input-event-key :bind
  "get_key_label_with_modifiers" :hash 1585896689)
 key)

(defgmethod
 (input-event-key+as-text-keycode :class 'input-event-key :bind
  "as_text_keycode" :hash 201670096)
 string)

(defgmethod
 (input-event-key+as-text-physical-keycode :class 'input-event-key :bind
  "as_text_physical_keycode" :hash 201670096)
 string)

(defgmethod
 (input-event-key+as-text-key-label :class 'input-event-key :bind
  "as_text_key_label" :hash 201670096)
 string)

(defgmethod
 (input-event-key+as-text-location :class 'input-event-key :bind
  "as_text_location" :hash 201670096)
 string)