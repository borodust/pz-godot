(common-lisp:in-package :%godot)


(defgmethod
 (editor-resource-picker+%set-create-options :class 'editor-resource-picker
  :bind "_set_create_options" :hash 3975164845 :virtual common-lisp:t)
 :void (menu-node object))

(defgmethod
 (editor-resource-picker+%handle-menu-selected :class 'editor-resource-picker
  :bind "_handle_menu_selected" :hash 3067735520 :virtual common-lisp:t)
 bool (id int))

(defgmethod
 (editor-resource-picker+set-base-type :class 'editor-resource-picker :bind
  "set_base_type" :hash 83702148)
 :void (base-type string))

(defgmethod
 (editor-resource-picker+get-base-type :class 'editor-resource-picker :bind
  "get_base_type" :hash 201670096)
 string)

(defgmethod
 (editor-resource-picker+get-allowed-types :class 'editor-resource-picker :bind
  "get_allowed_types" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-resource-picker+set-edited-resource :class 'editor-resource-picker
  :bind "set_edited_resource" :hash 968641751)
 :void (resource resource))

(defgmethod
 (editor-resource-picker+get-edited-resource :class 'editor-resource-picker
  :bind "get_edited_resource" :hash 2674603643)
 resource)

(defgmethod
 (editor-resource-picker+set-toggle-mode :class 'editor-resource-picker :bind
  "set_toggle_mode" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (editor-resource-picker+is-toggle-mode :class 'editor-resource-picker :bind
  "is_toggle_mode" :hash 36873697)
 bool)

(defgmethod
 (editor-resource-picker+set-toggle-pressed :class 'editor-resource-picker
  :bind "set_toggle_pressed" :hash 2586408642)
 :void (pressed bool))

(defgmethod
 (editor-resource-picker+set-editable :class 'editor-resource-picker :bind
  "set_editable" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (editor-resource-picker+is-editable :class 'editor-resource-picker :bind
  "is_editable" :hash 36873697)
 bool)