(common-lisp:in-package :%godot)


(defgmethod
 (editor-spin-slider+set-label :class 'editor-spin-slider :bind "set_label"
  :hash 83702148)
 :void (label string))

(defgmethod
 (editor-spin-slider+get-label :class 'editor-spin-slider :bind "get_label"
  :hash 201670096)
 string)

(defgmethod
 (editor-spin-slider+set-suffix :class 'editor-spin-slider :bind "set_suffix"
  :hash 83702148)
 :void (suffix string))

(defgmethod
 (editor-spin-slider+get-suffix :class 'editor-spin-slider :bind "get_suffix"
  :hash 201670096)
 string)

(defgmethod
 (editor-spin-slider+set-read-only :class 'editor-spin-slider :bind
  "set_read_only" :hash 2586408642)
 :void (read-only bool))

(defgmethod
 (editor-spin-slider+is-read-only :class 'editor-spin-slider :bind
  "is_read_only" :hash 36873697)
 bool)

(defgmethod
 (editor-spin-slider+set-flat :class 'editor-spin-slider :bind "set_flat" :hash
  2586408642)
 :void (flat bool))

(defgmethod
 (editor-spin-slider+is-flat :class 'editor-spin-slider :bind "is_flat" :hash
  36873697)
 bool)

(defgmethod
 (editor-spin-slider+set-control-state :class 'editor-spin-slider :bind
  "set_control_state" :hash 1324557109)
 :void (state editor-spin-slider+control-state))

(defgmethod
 (editor-spin-slider+get-control-state :class 'editor-spin-slider :bind
  "get_control_state" :hash 3406006200)
 editor-spin-slider+control-state)

(defgmethod
 (editor-spin-slider+set-hide-slider :class 'editor-spin-slider :bind
  "set_hide_slider" :hash 2586408642)
 :void (hide-slider bool))

(defgmethod
 (editor-spin-slider+is-hiding-slider :class 'editor-spin-slider :bind
  "is_hiding_slider" :hash 36873697)
 bool)

(defgmethod
 (editor-spin-slider+set-editing-integer :class 'editor-spin-slider :bind
  "set_editing_integer" :hash 2586408642)
 :void (editing-integer bool))

(defgmethod
 (editor-spin-slider+is-editing-integer :class 'editor-spin-slider :bind
  "is_editing_integer" :hash 36873697)
 bool)