(common-lisp:in-package :%godot)


(defgmethod
 (reference-rect+get-border-color :class 'reference-rect :bind
  "get_border_color" :hash 3444240500)
 color)

(defgmethod
 (reference-rect+set-border-color :class 'reference-rect :bind
  "set_border_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (reference-rect+get-border-width :class 'reference-rect :bind
  "get_border_width" :hash 1740695150)
 float)

(defgmethod
 (reference-rect+set-border-width :class 'reference-rect :bind
  "set_border_width" :hash 373806689)
 :void (width float))

(defgmethod
 (reference-rect+get-editor-only :class 'reference-rect :bind "get_editor_only"
  :hash 36873697)
 bool)

(defgmethod
 (reference-rect+set-editor-only :class 'reference-rect :bind "set_editor_only"
  :hash 2586408642)
 :void (enabled bool))