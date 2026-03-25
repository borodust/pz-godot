(common-lisp:in-package :%godot)


(defgmethod
 (graph-frame+set-title :class 'graph-frame :bind "set_title" :hash 83702148)
 :void (title string))

(defgmethod
 (graph-frame+get-title :class 'graph-frame :bind "get_title" :hash 201670096)
 string)

(defgmethod
 (graph-frame+get-titlebar-hbox :class 'graph-frame :bind "get_titlebar_hbox"
  :hash 3590609951)
 hbox-container)

(defgmethod
 (graph-frame+set-autoshrink-enabled :class 'graph-frame :bind
  "set_autoshrink_enabled" :hash 2586408642)
 :void (shrink bool))

(defgmethod
 (graph-frame+is-autoshrink-enabled :class 'graph-frame :bind
  "is_autoshrink_enabled" :hash 36873697)
 bool)

(defgmethod
 (graph-frame+set-autoshrink-margin :class 'graph-frame :bind
  "set_autoshrink_margin" :hash 1286410249)
 :void (autoshrink-margin int))

(defgmethod
 (graph-frame+get-autoshrink-margin :class 'graph-frame :bind
  "get_autoshrink_margin" :hash 3905245786)
 int)

(defgmethod
 (graph-frame+set-drag-margin :class 'graph-frame :bind "set_drag_margin" :hash
  1286410249)
 :void (drag-margin int))

(defgmethod
 (graph-frame+get-drag-margin :class 'graph-frame :bind "get_drag_margin" :hash
  3905245786)
 int)

(defgmethod
 (graph-frame+set-tint-color-enabled :class 'graph-frame :bind
  "set_tint_color_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (graph-frame+is-tint-color-enabled :class 'graph-frame :bind
  "is_tint_color_enabled" :hash 36873697)
 bool)

(defgmethod
 (graph-frame+set-tint-color :class 'graph-frame :bind "set_tint_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (graph-frame+get-tint-color :class 'graph-frame :bind "get_tint_color" :hash
  3444240500)
 color)