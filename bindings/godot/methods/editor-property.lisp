(common-lisp:in-package :%godot)


(defgmethod
 (editor-property+%update-property :class 'editor-property :bind
  "_update_property" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-property+%set-read-only :class 'editor-property :bind "_set_read_only"
  :hash 2586408642 :virtual common-lisp:t)
 :void (read-only bool))

(defgmethod
 (editor-property+set-label :class 'editor-property :bind "set_label" :hash
  83702148)
 :void (text string))

(defgmethod
 (editor-property+get-label :class 'editor-property :bind "get_label" :hash
  201670096)
 string)

(defgmethod
 (editor-property+set-read-only :class 'editor-property :bind "set_read_only"
  :hash 2586408642)
 :void (read-only bool))

(defgmethod
 (editor-property+is-read-only :class 'editor-property :bind "is_read_only"
  :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-draw-label :class 'editor-property :bind "set_draw_label"
  :hash 2586408642)
 :void (draw-label bool))

(defgmethod
 (editor-property+is-draw-label :class 'editor-property :bind "is_draw_label"
  :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-draw-background :class 'editor-property :bind
  "set_draw_background" :hash 2586408642)
 :void (draw-background bool))

(defgmethod
 (editor-property+is-draw-background :class 'editor-property :bind
  "is_draw_background" :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-checkable :class 'editor-property :bind "set_checkable"
  :hash 2586408642)
 :void (checkable bool))

(defgmethod
 (editor-property+is-checkable :class 'editor-property :bind "is_checkable"
  :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-checked :class 'editor-property :bind "set_checked" :hash
  2586408642)
 :void (checked bool))

(defgmethod
 (editor-property+is-checked :class 'editor-property :bind "is_checked" :hash
  36873697)
 bool)

(defgmethod
 (editor-property+set-draw-warning :class 'editor-property :bind
  "set_draw_warning" :hash 2586408642)
 :void (draw-warning bool))

(defgmethod
 (editor-property+is-draw-warning :class 'editor-property :bind
  "is_draw_warning" :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-keying :class 'editor-property :bind "set_keying" :hash
  2586408642)
 :void (keying bool))

(defgmethod
 (editor-property+is-keying :class 'editor-property :bind "is_keying" :hash
  36873697)
 bool)

(defgmethod
 (editor-property+set-deletable :class 'editor-property :bind "set_deletable"
  :hash 2586408642)
 :void (deletable bool))

(defgmethod
 (editor-property+is-deletable :class 'editor-property :bind "is_deletable"
  :hash 36873697)
 bool)

(defgmethod
 (editor-property+get-edited-property :class 'editor-property :bind
  "get_edited_property" :hash 2002593661)
 string-name)

(defgmethod
 (editor-property+get-edited-object :class 'editor-property :bind
  "get_edited_object" :hash 2050059866)
 object)

(defgmethod
 (editor-property+update-property :class 'editor-property :bind
  "update_property" :hash 3218959716)
 :void)

(defgmethod
 (editor-property+add-focusable :class 'editor-property :bind "add_focusable"
  :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-property+set-bottom-editor :class 'editor-property :bind
  "set_bottom_editor" :hash 1496901182)
 :void (editor control))

(defgmethod
 (editor-property+set-selectable :class 'editor-property :bind "set_selectable"
  :hash 2586408642)
 :void (selectable bool))

(defgmethod
 (editor-property+is-selectable :class 'editor-property :bind "is_selectable"
  :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-use-folding :class 'editor-property :bind
  "set_use_folding" :hash 2586408642)
 :void (use-folding bool))

(defgmethod
 (editor-property+is-using-folding :class 'editor-property :bind
  "is_using_folding" :hash 36873697)
 bool)

(defgmethod
 (editor-property+set-name-split-ratio :class 'editor-property :bind
  "set_name_split_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (editor-property+get-name-split-ratio :class 'editor-property :bind
  "get_name_split_ratio" :hash 1740695150)
 float)

(defgmethod
 (editor-property+deselect :class 'editor-property :bind "deselect" :hash
  3218959716)
 :void)

(defgmethod
 (editor-property+is-selected :class 'editor-property :bind "is_selected" :hash
  36873697)
 bool)

(defgmethod
 (editor-property+select :class 'editor-property :bind "select" :hash
  1025054187)
 :void (focusable int))

(defgmethod
 (editor-property+set-object-and-property :class 'editor-property :bind
  "set_object_and_property" :hash 4157606280)
 :void (object object) (property string-name))

(defgmethod
 (editor-property+set-label-reference :class 'editor-property :bind
  "set_label_reference" :hash 1496901182)
 :void (control control))

(defgmethod
 (editor-property+emit-changed :class 'editor-property :bind "emit_changed"
  :hash 1822500399)
 :void (property string-name) (value variant) (field string-name)
 (changing bool))