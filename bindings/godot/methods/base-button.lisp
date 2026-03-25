(common-lisp:in-package :%godot)


(defgmethod
 (base-button+-pressed :class 'base-button :bind "_pressed" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (base-button+-toggled :class 'base-button :bind "_toggled" :hash 2586408642
  :virtual common-lisp:t)
 :void (toggled-on bool))

(defgmethod
 (base-button+set-pressed :class 'base-button :bind "set_pressed" :hash
  2586408642)
 :void (pressed bool))

(defgmethod
 (base-button+is-pressed :class 'base-button :bind "is_pressed" :hash 36873697)
 bool)

(defgmethod
 (base-button+set-pressed-no-signal :class 'base-button :bind
  "set_pressed_no_signal" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (base-button+is-hovered :class 'base-button :bind "is_hovered" :hash 36873697)
 bool)

(defgmethod
 (base-button+set-toggle-mode :class 'base-button :bind "set_toggle_mode" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (base-button+is-toggle-mode :class 'base-button :bind "is_toggle_mode" :hash
  36873697)
 bool)

(defgmethod
 (base-button+set-shortcut-in-tooltip :class 'base-button :bind
  "set_shortcut_in_tooltip" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (base-button+is-shortcut-in-tooltip-enabled :class 'base-button :bind
  "is_shortcut_in_tooltip_enabled" :hash 36873697)
 bool)

(defgmethod
 (base-button+set-disabled :class 'base-button :bind "set_disabled" :hash
  2586408642)
 :void (disabled bool))

(defgmethod
 (base-button+is-disabled :class 'base-button :bind "is_disabled" :hash
  36873697)
 bool)

(defgmethod
 (base-button+set-action-mode :class 'base-button :bind "set_action_mode" :hash
  1985162088)
 :void (mode base-button+action-mode))

(defgmethod
 (base-button+get-action-mode :class 'base-button :bind "get_action_mode" :hash
  2589712189)
 base-button+action-mode)

(defgmethod
 (base-button+set-button-mask :class 'base-button :bind "set_button_mask" :hash
  3950145251)
 :void (mask mouse-button-mask))

(defgmethod
 (base-button+get-button-mask :class 'base-button :bind "get_button_mask" :hash
  2512161324)
 mouse-button-mask)

(defgmethod
 (base-button+get-draw-mode :class 'base-button :bind "get_draw_mode" :hash
  2492721305)
 base-button+draw-mode)

(defgmethod
 (base-button+set-keep-pressed-outside :class 'base-button :bind
  "set_keep_pressed_outside" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (base-button+is-keep-pressed-outside :class 'base-button :bind
  "is_keep_pressed_outside" :hash 36873697)
 bool)

(defgmethod
 (base-button+set-shortcut-feedback :class 'base-button :bind
  "set_shortcut_feedback" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (base-button+is-shortcut-feedback :class 'base-button :bind
  "is_shortcut_feedback" :hash 36873697)
 bool)

(defgmethod
 (base-button+set-shortcut :class 'base-button :bind "set_shortcut" :hash
  857163497)
 :void (shortcut shortcut))

(defgmethod
 (base-button+get-shortcut :class 'base-button :bind "get_shortcut" :hash
  3415666916)
 shortcut)

(defgmethod
 (base-button+set-button-group :class 'base-button :bind "set_button_group"
  :hash 1794463739)
 :void (button-group button-group))

(defgmethod
 (base-button+get-button-group :class 'base-button :bind "get_button_group"
  :hash 281644053)
 button-group)