(common-lisp:in-package :%godot)


(defgmethod
 (color-picker-button+set-pick-color :class 'color-picker-button :bind
  "set_pick_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (color-picker-button+get-pick-color :class 'color-picker-button :bind
  "get_pick_color" :hash 3444240500)
 color)

(defgmethod
 (color-picker-button+get-picker :class 'color-picker-button :bind "get_picker"
  :hash 331835996)
 color-picker)

(defgmethod
 (color-picker-button+get-popup :class 'color-picker-button :bind "get_popup"
  :hash 1322440207)
 popup-panel)

(defgmethod
 (color-picker-button+set-edit-alpha :class 'color-picker-button :bind
  "set_edit_alpha" :hash 2586408642)
 :void (show bool))

(defgmethod
 (color-picker-button+is-editing-alpha :class 'color-picker-button :bind
  "is_editing_alpha" :hash 36873697)
 bool)

(defgmethod
 (color-picker-button+set-edit-intensity :class 'color-picker-button :bind
  "set_edit_intensity" :hash 2586408642)
 :void (show bool))

(defgmethod
 (color-picker-button+is-editing-intensity :class 'color-picker-button :bind
  "is_editing_intensity" :hash 36873697)
 bool)