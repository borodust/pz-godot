(common-lisp:in-package :%godot)


(defgmethod
 (line-edit+has-ime-text :class 'line-edit :bind "has_ime_text" :hash 36873697)
 bool)

(defgmethod
 (line-edit+cancel-ime :class 'line-edit :bind "cancel_ime" :hash 3218959716)
 :void)

(defgmethod
 (line-edit+apply-ime :class 'line-edit :bind "apply_ime" :hash 3218959716)
 :void)

(defgmethod
 (line-edit+set-horizontal-alignment :class 'line-edit :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (line-edit+get-horizontal-alignment :class 'line-edit :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod (line-edit+edit :class 'line-edit :bind "edit" :hash 107499316)
 :void (hide-focus bool))

(defgmethod
 (line-edit+unedit :class 'line-edit :bind "unedit" :hash 3218959716) :void)

(defgmethod
 (line-edit+is-editing :class 'line-edit :bind "is_editing" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-keep-editing-on-text-submit :class 'line-edit :bind
  "set_keep_editing_on_text_submit" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-editing-kept-on-text-submit :class 'line-edit :bind
  "is_editing_kept_on_text_submit" :hash 36873697)
 bool)

(defgmethod (line-edit+clear :class 'line-edit :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (line-edit+select :class 'line-edit :bind "select" :hash 1328111411) :void
 (from int) (to int))

(defgmethod
 (line-edit+select-all :class 'line-edit :bind "select_all" :hash 3218959716)
 :void)

(defgmethod
 (line-edit+deselect :class 'line-edit :bind "deselect" :hash 3218959716) :void)

(defgmethod
 (line-edit+has-undo :class 'line-edit :bind "has_undo" :hash 36873697) bool)

(defgmethod
 (line-edit+has-redo :class 'line-edit :bind "has_redo" :hash 36873697) bool)

(defgmethod
 (line-edit+has-selection :class 'line-edit :bind "has_selection" :hash
  36873697)
 bool)

(defgmethod
 (line-edit+get-selected-text :class 'line-edit :bind "get_selected_text" :hash
  2841200299)
 string)

(defgmethod
 (line-edit+get-selection-from-column :class 'line-edit :bind
  "get_selection_from_column" :hash 3905245786)
 int)

(defgmethod
 (line-edit+get-selection-to-column :class 'line-edit :bind
  "get_selection_to_column" :hash 3905245786)
 int)

(defgmethod
 (line-edit+set-text :class 'line-edit :bind "set_text" :hash 83702148) :void
 (text string))

(defgmethod
 (line-edit+get-text :class 'line-edit :bind "get_text" :hash 201670096) string)

(defgmethod
 (line-edit+get-draw-control-chars :class 'line-edit :bind
  "get_draw_control_chars" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-draw-control-chars :class 'line-edit :bind
  "set_draw_control_chars" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+set-text-direction :class 'line-edit :bind "set_text_direction"
  :hash 119160795)
 :void (direction control+text-direction))

(defgmethod
 (line-edit+get-text-direction :class 'line-edit :bind "get_text_direction"
  :hash 797257663)
 control+text-direction)

(defgmethod
 (line-edit+set-language :class 'line-edit :bind "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (line-edit+get-language :class 'line-edit :bind "get_language" :hash
  201670096)
 string)

(defgmethod
 (line-edit+set-structured-text-bidi-override :class 'line-edit :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (line-edit+get-structured-text-bidi-override :class 'line-edit :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (line-edit+set-structured-text-bidi-override-options :class 'line-edit :bind
  "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (line-edit+get-structured-text-bidi-override-options :class 'line-edit :bind
  "get_structured_text_bidi_override_options" :hash 3995934104)
 array)

(defgmethod
 (line-edit+set-placeholder :class 'line-edit :bind "set_placeholder" :hash
  83702148)
 :void (text string))

(defgmethod
 (line-edit+get-placeholder :class 'line-edit :bind "get_placeholder" :hash
  201670096)
 string)

(defgmethod
 (line-edit+set-caret-column :class 'line-edit :bind "set_caret_column" :hash
  1286410249)
 :void (position int))

(defgmethod
 (line-edit+get-caret-column :class 'line-edit :bind "get_caret_column" :hash
  3905245786)
 int)

(defgmethod
 (line-edit+get-next-composite-character-column :class 'line-edit :bind
  "get_next_composite_character_column" :hash 923996154)
 int (column int))

(defgmethod
 (line-edit+get-previous-composite-character-column :class 'line-edit :bind
  "get_previous_composite_character_column" :hash 923996154)
 int (column int))

(defgmethod
 (line-edit+get-scroll-offset :class 'line-edit :bind "get_scroll_offset" :hash
  1740695150)
 float)

(defgmethod
 (line-edit+set-expand-to-text-length-enabled :class 'line-edit :bind
  "set_expand_to_text_length_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-expand-to-text-length-enabled :class 'line-edit :bind
  "is_expand_to_text_length_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-caret-blink-enabled :class 'line-edit :bind
  "set_caret_blink_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-caret-blink-enabled :class 'line-edit :bind
  "is_caret_blink_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-caret-mid-grapheme-enabled :class 'line-edit :bind
  "set_caret_mid_grapheme_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-caret-mid-grapheme-enabled :class 'line-edit :bind
  "is_caret_mid_grapheme_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-caret-force-displayed :class 'line-edit :bind
  "set_caret_force_displayed" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-caret-force-displayed :class 'line-edit :bind
  "is_caret_force_displayed" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-caret-blink-interval :class 'line-edit :bind
  "set_caret_blink_interval" :hash 373806689)
 :void (interval float))

(defgmethod
 (line-edit+get-caret-blink-interval :class 'line-edit :bind
  "get_caret_blink_interval" :hash 1740695150)
 float)

(defgmethod
 (line-edit+set-max-length :class 'line-edit :bind "set_max_length" :hash
  1286410249)
 :void (chars int))

(defgmethod
 (line-edit+get-max-length :class 'line-edit :bind "get_max_length" :hash
  3905245786)
 int)

(defgmethod
 (line-edit+insert-text-at-caret :class 'line-edit :bind "insert_text_at_caret"
  :hash 83702148)
 :void (text string))

(defgmethod
 (line-edit+delete-char-at-caret :class 'line-edit :bind "delete_char_at_caret"
  :hash 3218959716)
 :void)

(defgmethod
 (line-edit+delete-text :class 'line-edit :bind "delete_text" :hash 3937882851)
 :void (from-column int) (to-column int))

(defgmethod
 (line-edit+set-editable :class 'line-edit :bind "set_editable" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-editable :class 'line-edit :bind "is_editable" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-secret :class 'line-edit :bind "set_secret" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-secret :class 'line-edit :bind "is_secret" :hash 36873697) bool)

(defgmethod
 (line-edit+set-secret-character :class 'line-edit :bind "set_secret_character"
  :hash 83702148)
 :void (character string))

(defgmethod
 (line-edit+get-secret-character :class 'line-edit :bind "get_secret_character"
  :hash 201670096)
 string)

(defgmethod
 (line-edit+menu-option :class 'line-edit :bind "menu_option" :hash 1286410249)
 :void (option int))

(defgmethod
 (line-edit+get-menu :class 'line-edit :bind "get_menu" :hash 229722558)
 popup-menu)

(defgmethod
 (line-edit+is-menu-visible :class 'line-edit :bind "is_menu_visible" :hash
  36873697)
 bool)

(defgmethod
 (line-edit+set-context-menu-enabled :class 'line-edit :bind
  "set_context_menu_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-context-menu-enabled :class 'line-edit :bind
  "is_context_menu_enabled" :hash 2240911060)
 bool)

(defgmethod
 (line-edit+set-emoji-menu-enabled :class 'line-edit :bind
  "set_emoji_menu_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-emoji-menu-enabled :class 'line-edit :bind
  "is_emoji_menu_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-backspace-deletes-composite-character-enabled :class 'line-edit
  :bind "set_backspace_deletes_composite_character_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-backspace-deletes-composite-character-enabled :class 'line-edit
  :bind "is_backspace_deletes_composite_character_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-virtual-keyboard-enabled :class 'line-edit :bind
  "set_virtual_keyboard_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-virtual-keyboard-enabled :class 'line-edit :bind
  "is_virtual_keyboard_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-virtual-keyboard-show-on-focus :class 'line-edit :bind
  "set_virtual_keyboard_show_on_focus" :hash 2586408642)
 :void (show-on-focus bool))

(defgmethod
 (line-edit+get-virtual-keyboard-show-on-focus :class 'line-edit :bind
  "get_virtual_keyboard_show_on_focus" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-virtual-keyboard-type :class 'line-edit :bind
  "set_virtual_keyboard_type" :hash 2696893573)
 :void (type line-edit+virtual-keyboard-type))

(defgmethod
 (line-edit+get-virtual-keyboard-type :class 'line-edit :bind
  "get_virtual_keyboard_type" :hash 1928699316)
 line-edit+virtual-keyboard-type)

(defgmethod
 (line-edit+set-clear-button-enabled :class 'line-edit :bind
  "set_clear_button_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-clear-button-enabled :class 'line-edit :bind
  "is_clear_button_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-shortcut-keys-enabled :class 'line-edit :bind
  "set_shortcut_keys_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-shortcut-keys-enabled :class 'line-edit :bind
  "is_shortcut_keys_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-middle-mouse-paste-enabled :class 'line-edit :bind
  "set_middle_mouse_paste_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-middle-mouse-paste-enabled :class 'line-edit :bind
  "is_middle_mouse_paste_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-selecting-enabled :class 'line-edit :bind
  "set_selecting_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-selecting-enabled :class 'line-edit :bind "is_selecting_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-deselect-on-focus-loss-enabled :class 'line-edit :bind
  "set_deselect_on_focus_loss_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-deselect-on-focus-loss-enabled :class 'line-edit :bind
  "is_deselect_on_focus_loss_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-drag-and-drop-selection-enabled :class 'line-edit :bind
  "set_drag_and_drop_selection_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (line-edit+is-drag-and-drop-selection-enabled :class 'line-edit :bind
  "is_drag_and_drop_selection_enabled" :hash 36873697)
 bool)

(defgmethod
 (line-edit+set-right-icon :class 'line-edit :bind "set_right_icon" :hash
  4051416890)
 :void (icon texture-2d))

(defgmethod
 (line-edit+get-right-icon :class 'line-edit :bind "get_right_icon" :hash
  255860311)
 texture-2d)

(defgmethod
 (line-edit+set-icon-expand-mode :class 'line-edit :bind "set_icon_expand_mode"
  :hash 3019903192)
 :void (mode line-edit+expand-mode))

(defgmethod
 (line-edit+get-icon-expand-mode :class 'line-edit :bind "get_icon_expand_mode"
  :hash 3273584435)
 line-edit+expand-mode)

(defgmethod
 (line-edit+set-right-icon-scale :class 'line-edit :bind "set_right_icon_scale"
  :hash 373806689)
 :void (scale float))

(defgmethod
 (line-edit+get-right-icon-scale :class 'line-edit :bind "get_right_icon_scale"
  :hash 1740695150)
 float)

(defgmethod
 (line-edit+set-flat :class 'line-edit :bind "set_flat" :hash 2586408642) :void
 (enabled bool))

(defgmethod
 (line-edit+is-flat :class 'line-edit :bind "is_flat" :hash 36873697) bool)

(defgmethod
 (line-edit+set-select-all-on-focus :class 'line-edit :bind
  "set_select_all_on_focus" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (line-edit+is-select-all-on-focus :class 'line-edit :bind
  "is_select_all_on_focus" :hash 36873697)
 bool)