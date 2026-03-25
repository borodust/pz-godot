(common-lisp:in-package :%godot)


(defgmethod
 (split-container+set-split-offsets :class 'split-container :bind
  "set_split_offsets" :hash 3614634198)
 :void (offsets packed-int-32array))

(defgmethod
 (split-container+get-split-offsets :class 'split-container :bind
  "get_split_offsets" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (split-container+clamp-split-offset :class 'split-container :bind
  "clamp_split_offset" :hash 1995695955)
 :void (priority-index int))

(defgmethod
 (split-container+set-collapsed :class 'split-container :bind "set_collapsed"
  :hash 2586408642)
 :void (collapsed bool))

(defgmethod
 (split-container+is-collapsed :class 'split-container :bind "is_collapsed"
  :hash 36873697)
 bool)

(defgmethod
 (split-container+set-dragger-visibility :class 'split-container :bind
  "set_dragger_visibility" :hash 1168273952)
 :void (mode split-container+dragger-visibility))

(defgmethod
 (split-container+get-dragger-visibility :class 'split-container :bind
  "get_dragger_visibility" :hash 967297479)
 split-container+dragger-visibility)

(defgmethod
 (split-container+set-vertical :class 'split-container :bind "set_vertical"
  :hash 2586408642)
 :void (vertical bool))

(defgmethod
 (split-container+is-vertical :class 'split-container :bind "is_vertical" :hash
  36873697)
 bool)

(defgmethod
 (split-container+set-dragging-enabled :class 'split-container :bind
  "set_dragging_enabled" :hash 2586408642)
 :void (dragging-enabled bool))

(defgmethod
 (split-container+is-dragging-enabled :class 'split-container :bind
  "is_dragging_enabled" :hash 36873697)
 bool)

(defgmethod
 (split-container+set-drag-area-margin-begin :class 'split-container :bind
  "set_drag_area_margin_begin" :hash 1286410249)
 :void (margin int))

(defgmethod
 (split-container+get-drag-area-margin-begin :class 'split-container :bind
  "get_drag_area_margin_begin" :hash 3905245786)
 int)

(defgmethod
 (split-container+set-drag-area-margin-end :class 'split-container :bind
  "set_drag_area_margin_end" :hash 1286410249)
 :void (margin int))

(defgmethod
 (split-container+get-drag-area-margin-end :class 'split-container :bind
  "get_drag_area_margin_end" :hash 3905245786)
 int)

(defgmethod
 (split-container+set-drag-area-offset :class 'split-container :bind
  "set_drag_area_offset" :hash 1286410249)
 :void (offset int))

(defgmethod
 (split-container+get-drag-area-offset :class 'split-container :bind
  "get_drag_area_offset" :hash 3905245786)
 int)

(defgmethod
 (split-container+set-drag-area-highlight-in-editor :class 'split-container
  :bind "set_drag_area_highlight_in_editor" :hash 2586408642)
 :void (drag-area-highlight-in-editor bool))

(defgmethod
 (split-container+is-drag-area-highlight-in-editor-enabled :class
  'split-container :bind "is_drag_area_highlight_in_editor_enabled" :hash
  36873697)
 bool)

(defgmethod
 (split-container+get-drag-area-controls :class 'split-container :bind
  "get_drag_area_controls" :hash 2915620761)
 array)

(defgmethod
 (split-container+set-touch-dragger-enabled :class 'split-container :bind
  "set_touch_dragger_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (split-container+is-touch-dragger-enabled :class 'split-container :bind
  "is_touch_dragger_enabled" :hash 36873697)
 bool)

(defgmethod
 (split-container+get-drag-area-control :class 'split-container :bind
  "get_drag_area_control" :hash 829782337)
 control)

(defgmethod
 (split-container+set-split-offset :class 'split-container :bind
  "set_split_offset" :hash 1286410249)
 :void (offset int))

(defgmethod
 (split-container+get-split-offset :class 'split-container :bind
  "get_split_offset" :hash 3905245786)
 int)