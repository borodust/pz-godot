(common-lisp:in-package :%godot)


(defgmethod
 (text-edit+%handle-unicode-input :class 'text-edit :bind
  "_handle_unicode_input" :hash 3937882851 :virtual common-lisp:t)
 :void (unicode-char int) (caret-index int))

(defgmethod
 (text-edit+%backspace :class 'text-edit :bind "_backspace" :hash 1286410249
  :virtual common-lisp:t)
 :void (caret-index int))

(defgmethod
 (text-edit+%cut :class 'text-edit :bind "_cut" :hash 1286410249 :virtual
  common-lisp:t)
 :void (caret-index int))

(defgmethod
 (text-edit+%copy :class 'text-edit :bind "_copy" :hash 1286410249 :virtual
  common-lisp:t)
 :void (caret-index int))

(defgmethod
 (text-edit+%paste :class 'text-edit :bind "_paste" :hash 1286410249 :virtual
  common-lisp:t)
 :void (caret-index int))

(defgmethod
 (text-edit+%paste-primary-clipboard :class 'text-edit :bind
  "_paste_primary_clipboard" :hash 1286410249 :virtual common-lisp:t)
 :void (caret-index int))

(defgmethod
 (text-edit+has-ime-text :class 'text-edit :bind "has_ime_text" :hash 36873697)
 bool)

(defgmethod
 (text-edit+cancel-ime :class 'text-edit :bind "cancel_ime" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+apply-ime :class 'text-edit :bind "apply_ime" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+set-editable :class 'text-edit :bind "set_editable" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-editable :class 'text-edit :bind "is_editable" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-text-direction :class 'text-edit :bind "set_text_direction"
  :hash 119160795)
 :void (direction control+text-direction))

(defgmethod
 (text-edit+get-text-direction :class 'text-edit :bind "get_text_direction"
  :hash 797257663)
 control+text-direction)

(defgmethod
 (text-edit+set-language :class 'text-edit :bind "set_language" :hash 83702148)
 :void (language string))

(defgmethod
 (text-edit+get-language :class 'text-edit :bind "get_language" :hash
  201670096)
 string)

(defgmethod
 (text-edit+set-structured-text-bidi-override :class 'text-edit :bind
  "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (text-edit+get-structured-text-bidi-override :class 'text-edit :bind
  "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (text-edit+set-structured-text-bidi-override-options :class 'text-edit :bind
  "set_structured_text_bidi_override_options" :hash 381264803)
 :void (args array))

(defgmethod
 (text-edit+get-structured-text-bidi-override-options :class 'text-edit :bind
  "get_structured_text_bidi_override_options" :hash 3995934104)
 array)

(defgmethod
 (text-edit+set-tab-size :class 'text-edit :bind "set_tab_size" :hash
  1286410249)
 :void (size int))

(defgmethod
 (text-edit+get-tab-size :class 'text-edit :bind "get_tab_size" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+set-indent-wrapped-lines :class 'text-edit :bind
  "set_indent_wrapped_lines" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-indent-wrapped-lines :class 'text-edit :bind
  "is_indent_wrapped_lines" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-tab-input-mode :class 'text-edit :bind "set_tab_input_mode"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+get-tab-input-mode :class 'text-edit :bind "get_tab_input_mode"
  :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-overtype-mode-enabled :class 'text-edit :bind
  "set_overtype_mode_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-overtype-mode-enabled :class 'text-edit :bind
  "is_overtype_mode_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-context-menu-enabled :class 'text-edit :bind
  "set_context_menu_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-context-menu-enabled :class 'text-edit :bind
  "is_context_menu_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-emoji-menu-enabled :class 'text-edit :bind
  "set_emoji_menu_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-emoji-menu-enabled :class 'text-edit :bind
  "is_emoji_menu_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-backspace-deletes-composite-character-enabled :class 'text-edit
  :bind "set_backspace_deletes_composite_character_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-backspace-deletes-composite-character-enabled :class 'text-edit
  :bind "is_backspace_deletes_composite_character_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-shortcut-keys-enabled :class 'text-edit :bind
  "set_shortcut_keys_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-shortcut-keys-enabled :class 'text-edit :bind
  "is_shortcut_keys_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-virtual-keyboard-enabled :class 'text-edit :bind
  "set_virtual_keyboard_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-virtual-keyboard-enabled :class 'text-edit :bind
  "is_virtual_keyboard_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-virtual-keyboard-show-on-focus :class 'text-edit :bind
  "set_virtual_keyboard_show_on_focus" :hash 2586408642)
 :void (show-on-focus bool))

(defgmethod
 (text-edit+get-virtual-keyboard-show-on-focus :class 'text-edit :bind
  "get_virtual_keyboard_show_on_focus" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-middle-mouse-paste-enabled :class 'text-edit :bind
  "set_middle_mouse_paste_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-middle-mouse-paste-enabled :class 'text-edit :bind
  "is_middle_mouse_paste_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-empty-selection-clipboard-enabled :class 'text-edit :bind
  "set_empty_selection_clipboard_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-empty-selection-clipboard-enabled :class 'text-edit :bind
  "is_empty_selection_clipboard_enabled" :hash 36873697)
 bool)

(defgmethod (text-edit+clear :class 'text-edit :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+set-text :class 'text-edit :bind "set_text" :hash 83702148) :void
 (text string))

(defgmethod
 (text-edit+get-text :class 'text-edit :bind "get_text" :hash 201670096) string)

(defgmethod
 (text-edit+get-line-count :class 'text-edit :bind "get_line_count" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+set-placeholder :class 'text-edit :bind "set_placeholder" :hash
  83702148)
 :void (text string))

(defgmethod
 (text-edit+get-placeholder :class 'text-edit :bind "get_placeholder" :hash
  201670096)
 string)

(defgmethod
 (text-edit+set-line :class 'text-edit :bind "set_line" :hash 501894301) :void
 (line int) (new-text string))

(defgmethod
 (text-edit+get-line :class 'text-edit :bind "get_line" :hash 844755477) string
 (line int))

(defgmethod
 (text-edit+get-line-with-ime :class 'text-edit :bind "get_line_with_ime" :hash
  844755477)
 string (line int))

(defgmethod
 (text-edit+get-line-width :class 'text-edit :bind "get_line_width" :hash
  688195400)
 int (line int) (wrap-index int))

(defgmethod
 (text-edit+get-line-height :class 'text-edit :bind "get_line_height" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+get-indent-level :class 'text-edit :bind "get_indent_level" :hash
  923996154)
 int (line int))

(defgmethod
 (text-edit+get-first-non-whitespace-column :class 'text-edit :bind
  "get_first_non_whitespace_column" :hash 923996154)
 int (line int))

(defgmethod
 (text-edit+swap-lines :class 'text-edit :bind "swap_lines" :hash 3937882851)
 :void (from-line int) (to-line int))

(defgmethod
 (text-edit+insert-line-at :class 'text-edit :bind "insert_line_at" :hash
  501894301)
 :void (line int) (text string))

(defgmethod
 (text-edit+remove-line-at :class 'text-edit :bind "remove_line_at" :hash
  972357352)
 :void (line int) (move-carets-down bool))

(defgmethod
 (text-edit+insert-text-at-caret :class 'text-edit :bind "insert_text_at_caret"
  :hash 2697778442)
 :void (text string) (caret-index int))

(defgmethod
 (text-edit+insert-text :class 'text-edit :bind "insert_text" :hash 1881564334)
 :void (text string) (line int) (column int) (before-selection-begin bool)
 (before-selection-end bool))

(defgmethod
 (text-edit+remove-text :class 'text-edit :bind "remove_text" :hash 4275841770)
 :void (from-line int) (from-column int) (to-line int) (to-column int))

(defgmethod
 (text-edit+get-last-unhidden-line :class 'text-edit :bind
  "get_last_unhidden_line" :hash 3905245786)
 int)

(defgmethod
 (text-edit+get-next-visible-line-offset-from :class 'text-edit :bind
  "get_next_visible_line_offset_from" :hash 3175239445)
 int (line int) (visible-amount int))

(defgmethod
 (text-edit+get-next-visible-line-index-offset-from :class 'text-edit :bind
  "get_next_visible_line_index_offset_from" :hash 3386475622)
 vector-2i (line int) (wrap-index int) (visible-amount int))

(defgmethod
 (text-edit+backspace :class 'text-edit :bind "backspace" :hash 1025054187)
 :void (caret-index int))

(defgmethod (text-edit+cut :class 'text-edit :bind "cut" :hash 1025054187)
 :void (caret-index int))

(defgmethod (text-edit+copy :class 'text-edit :bind "copy" :hash 1025054187)
 :void (caret-index int))

(defgmethod (text-edit+paste :class 'text-edit :bind "paste" :hash 1025054187)
 :void (caret-index int))

(defgmethod
 (text-edit+paste-primary-clipboard :class 'text-edit :bind
  "paste_primary_clipboard" :hash 1025054187)
 :void (caret-index int))

(defgmethod
 (text-edit+start-action :class 'text-edit :bind "start_action" :hash
  2834827583)
 :void (action text-edit+edit-action))

(defgmethod
 (text-edit+end-action :class 'text-edit :bind "end_action" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+begin-complex-operation :class 'text-edit :bind
  "begin_complex_operation" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+end-complex-operation :class 'text-edit :bind
  "end_complex_operation" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+has-undo :class 'text-edit :bind "has_undo" :hash 36873697) bool)

(defgmethod
 (text-edit+has-redo :class 'text-edit :bind "has_redo" :hash 36873697) bool)

(defgmethod (text-edit+undo :class 'text-edit :bind "undo" :hash 3218959716)
 :void)

(defgmethod (text-edit+redo :class 'text-edit :bind "redo" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+clear-undo-history :class 'text-edit :bind "clear_undo_history"
  :hash 3218959716)
 :void)

(defgmethod
 (text-edit+tag-saved-version :class 'text-edit :bind "tag_saved_version" :hash
  3218959716)
 :void)

(defgmethod
 (text-edit+get-version :class 'text-edit :bind "get_version" :hash 3905245786)
 int)

(defgmethod
 (text-edit+get-saved-version :class 'text-edit :bind "get_saved_version" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+set-search-text :class 'text-edit :bind "set_search_text" :hash
  83702148)
 :void (search-text string))

(defgmethod
 (text-edit+set-search-flags :class 'text-edit :bind "set_search_flags" :hash
  1286410249)
 :void (flags int))

(defgmethod
 (text-edit+search :class 'text-edit :bind "search" :hash 1203739136) vector-2i
 (text string) (flags int) (from-line int) (from-column int))

(defgmethod
 (text-edit+set-tooltip-request-func :class 'text-edit :bind
  "set_tooltip_request_func" :hash 1611583062)
 :void (callback callable))

(defgmethod
 (text-edit+get-local-mouse-pos :class 'text-edit :bind "get_local_mouse_pos"
  :hash 3341600327)
 vector-2)

(defgmethod
 (text-edit+get-word-at-pos :class 'text-edit :bind "get_word_at_pos" :hash
  3674420000)
 string (position vector-2))

(defgmethod
 (text-edit+get-line-column-at-pos :class 'text-edit :bind
  "get_line_column_at_pos" :hash 3472935744)
 vector-2i (position vector-2i) (clamp-line bool) (clamp-column bool))

(defgmethod
 (text-edit+get-pos-at-line-column :class 'text-edit :bind
  "get_pos_at_line_column" :hash 410388347)
 vector-2i (line int) (column int))

(defgmethod
 (text-edit+get-rect-at-line-column :class 'text-edit :bind
  "get_rect_at_line_column" :hash 3256618057)
 rect-2i (line int) (column int))

(defgmethod
 (text-edit+get-minimap-line-at-pos :class 'text-edit :bind
  "get_minimap_line_at_pos" :hash 2485466453)
 int (position vector-2i))

(defgmethod
 (text-edit+is-dragging-cursor :class 'text-edit :bind "is_dragging_cursor"
  :hash 36873697)
 bool)

(defgmethod
 (text-edit+is-mouse-over-selection :class 'text-edit :bind
  "is_mouse_over_selection" :hash 1840282309)
 bool (edges bool) (caret-index int))

(defgmethod
 (text-edit+set-caret-type :class 'text-edit :bind "set_caret_type" :hash
  1211596914)
 :void (type text-edit+caret-type))

(defgmethod
 (text-edit+get-caret-type :class 'text-edit :bind "get_caret_type" :hash
  2830252959)
 text-edit+caret-type)

(defgmethod
 (text-edit+set-caret-blink-enabled :class 'text-edit :bind
  "set_caret_blink_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-caret-blink-enabled :class 'text-edit :bind
  "is_caret_blink_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-caret-blink-interval :class 'text-edit :bind
  "set_caret_blink_interval" :hash 373806689)
 :void (interval float))

(defgmethod
 (text-edit+get-caret-blink-interval :class 'text-edit :bind
  "get_caret_blink_interval" :hash 1740695150)
 float)

(defgmethod
 (text-edit+set-draw-caret-when-editable-disabled :class 'text-edit :bind
  "set_draw_caret_when_editable_disabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-drawing-caret-when-editable-disabled :class 'text-edit :bind
  "is_drawing_caret_when_editable_disabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-move-caret-on-right-click-enabled :class 'text-edit :bind
  "set_move_caret_on_right_click_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-move-caret-on-right-click-enabled :class 'text-edit :bind
  "is_move_caret_on_right_click_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-caret-mid-grapheme-enabled :class 'text-edit :bind
  "set_caret_mid_grapheme_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-caret-mid-grapheme-enabled :class 'text-edit :bind
  "is_caret_mid_grapheme_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-multiple-carets-enabled :class 'text-edit :bind
  "set_multiple_carets_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-multiple-carets-enabled :class 'text-edit :bind
  "is_multiple_carets_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+add-caret :class 'text-edit :bind "add_caret" :hash 50157827) int
 (line int) (column int))

(defgmethod
 (text-edit+remove-caret :class 'text-edit :bind "remove_caret" :hash
  1286410249)
 :void (caret int))

(defgmethod
 (text-edit+remove-secondary-carets :class 'text-edit :bind
  "remove_secondary_carets" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+get-caret-count :class 'text-edit :bind "get_caret_count" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+add-caret-at-carets :class 'text-edit :bind "add_caret_at_carets"
  :hash 2586408642)
 :void (below bool))

(defgmethod
 (text-edit+get-sorted-carets :class 'text-edit :bind "get_sorted_carets" :hash
  2131714034)
 packed-int-32array (include-ignored-carets bool))

(defgmethod
 (text-edit+collapse-carets :class 'text-edit :bind "collapse_carets" :hash
  228654177)
 :void (from-line int) (from-column int) (to-line int) (to-column int)
 (inclusive bool))

(defgmethod
 (text-edit+merge-overlapping-carets :class 'text-edit :bind
  "merge_overlapping_carets" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+begin-multicaret-edit :class 'text-edit :bind
  "begin_multicaret_edit" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+end-multicaret-edit :class 'text-edit :bind "end_multicaret_edit"
  :hash 3218959716)
 :void)

(defgmethod
 (text-edit+is-in-mulitcaret-edit :class 'text-edit :bind
  "is_in_mulitcaret_edit" :hash 36873697)
 bool)

(defgmethod
 (text-edit+multicaret-edit-ignore-caret :class 'text-edit :bind
  "multicaret_edit_ignore_caret" :hash 1116898809)
 bool (caret-index int))

(defgmethod
 (text-edit+is-caret-visible :class 'text-edit :bind "is_caret_visible" :hash
  1051549951)
 bool (caret-index int))

(defgmethod
 (text-edit+get-caret-draw-pos :class 'text-edit :bind "get_caret_draw_pos"
  :hash 478253731)
 vector-2 (caret-index int))

(defgmethod
 (text-edit+set-caret-line :class 'text-edit :bind "set_caret_line" :hash
  1302582944)
 :void (line int) (adjust-viewport bool) (can-be-hidden bool) (wrap-index int)
 (caret-index int))

(defgmethod
 (text-edit+get-caret-line :class 'text-edit :bind "get_caret_line" :hash
  1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+set-caret-column :class 'text-edit :bind "set_caret_column" :hash
  3796796178)
 :void (column int) (adjust-viewport bool) (caret-index int))

(defgmethod
 (text-edit+get-caret-column :class 'text-edit :bind "get_caret_column" :hash
  1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-next-composite-character-column :class 'text-edit :bind
  "get_next_composite_character_column" :hash 3175239445)
 int (line int) (column int))

(defgmethod
 (text-edit+get-previous-composite-character-column :class 'text-edit :bind
  "get_previous_composite_character_column" :hash 3175239445)
 int (line int) (column int))

(defgmethod
 (text-edit+get-caret-wrap-index :class 'text-edit :bind "get_caret_wrap_index"
  :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-word-under-caret :class 'text-edit :bind "get_word_under_caret"
  :hash 3929349208)
 string (caret-index int))

(defgmethod
 (text-edit+set-use-default-word-separators :class 'text-edit :bind
  "set_use_default_word_separators" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-default-word-separators-enabled :class 'text-edit :bind
  "is_default_word_separators_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-use-custom-word-separators :class 'text-edit :bind
  "set_use_custom_word_separators" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-custom-word-separators-enabled :class 'text-edit :bind
  "is_custom_word_separators_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-custom-word-separators :class 'text-edit :bind
  "set_custom_word_separators" :hash 83702148)
 :void (custom-word-separators string))

(defgmethod
 (text-edit+get-custom-word-separators :class 'text-edit :bind
  "get_custom_word_separators" :hash 201670096)
 string)

(defgmethod
 (text-edit+set-selecting-enabled :class 'text-edit :bind
  "set_selecting_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-selecting-enabled :class 'text-edit :bind "is_selecting_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-deselect-on-focus-loss-enabled :class 'text-edit :bind
  "set_deselect_on_focus_loss_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-deselect-on-focus-loss-enabled :class 'text-edit :bind
  "is_deselect_on_focus_loss_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-drag-and-drop-selection-enabled :class 'text-edit :bind
  "set_drag_and_drop_selection_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-drag-and-drop-selection-enabled :class 'text-edit :bind
  "is_drag_and_drop_selection_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-selection-mode :class 'text-edit :bind "set_selection_mode"
  :hash 1658801786)
 :void (mode text-edit+selection-mode))

(defgmethod
 (text-edit+get-selection-mode :class 'text-edit :bind "get_selection_mode"
  :hash 3750106938)
 text-edit+selection-mode)

(defgmethod
 (text-edit+select-all :class 'text-edit :bind "select_all" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+select-word-under-caret :class 'text-edit :bind
  "select_word_under_caret" :hash 1025054187)
 :void (caret-index int))

(defgmethod
 (text-edit+add-selection-for-next-occurrence :class 'text-edit :bind
  "add_selection_for_next_occurrence" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+skip-selection-for-next-occurrence :class 'text-edit :bind
  "skip_selection_for_next_occurrence" :hash 3218959716)
 :void)

(defgmethod
 (text-edit+select :class 'text-edit :bind "select" :hash 2560984452) :void
 (origin-line int) (origin-column int) (caret-line int) (caret-column int)
 (caret-index int))

(defgmethod
 (text-edit+has-selection :class 'text-edit :bind "has_selection" :hash
  2824505868)
 bool (caret-index int))

(defgmethod
 (text-edit+get-selected-text :class 'text-edit :bind "get_selected_text" :hash
  2309358862)
 string (caret-index int))

(defgmethod
 (text-edit+get-selection-at-line-column :class 'text-edit :bind
  "get_selection_at_line_column" :hash 1810224333)
 int (line int) (column int) (include-edges bool) (only-selections bool))

(defgmethod
 (text-edit+get-line-ranges-from-carets :class 'text-edit :bind
  "get_line_ranges_from_carets" :hash 2393089247)
 array (only-selections bool) (merge-adjacent bool))

(defgmethod
 (text-edit+get-selection-origin-line :class 'text-edit :bind
  "get_selection_origin_line" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-selection-origin-column :class 'text-edit :bind
  "get_selection_origin_column" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+set-selection-origin-line :class 'text-edit :bind
  "set_selection_origin_line" :hash 195434140)
 :void (line int) (can-be-hidden bool) (wrap-index int) (caret-index int))

(defgmethod
 (text-edit+set-selection-origin-column :class 'text-edit :bind
  "set_selection_origin_column" :hash 2230941749)
 :void (column int) (caret-index int))

(defgmethod
 (text-edit+get-selection-from-line :class 'text-edit :bind
  "get_selection_from_line" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-selection-from-column :class 'text-edit :bind
  "get_selection_from_column" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-selection-to-line :class 'text-edit :bind
  "get_selection_to_line" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-selection-to-column :class 'text-edit :bind
  "get_selection_to_column" :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+is-caret-after-selection-origin :class 'text-edit :bind
  "is_caret_after_selection_origin" :hash 1051549951)
 bool (caret-index int))

(defgmethod
 (text-edit+deselect :class 'text-edit :bind "deselect" :hash 1025054187) :void
 (caret-index int))

(defgmethod
 (text-edit+delete-selection :class 'text-edit :bind "delete_selection" :hash
  1025054187)
 :void (caret-index int))

(defgmethod
 (text-edit+set-line-wrapping-mode :class 'text-edit :bind
  "set_line_wrapping_mode" :hash 2525115309)
 :void (mode text-edit+line-wrapping-mode))

(defgmethod
 (text-edit+get-line-wrapping-mode :class 'text-edit :bind
  "get_line_wrapping_mode" :hash 3562716114)
 text-edit+line-wrapping-mode)

(defgmethod
 (text-edit+set-autowrap-mode :class 'text-edit :bind "set_autowrap_mode" :hash
  3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (text-edit+get-autowrap-mode :class 'text-edit :bind "get_autowrap_mode" :hash
  1549071663)
 text-server+autowrap-mode)

(defgmethod
 (text-edit+is-line-wrapped :class 'text-edit :bind "is_line_wrapped" :hash
  1116898809)
 bool (line int))

(defgmethod
 (text-edit+get-line-wrap-count :class 'text-edit :bind "get_line_wrap_count"
  :hash 923996154)
 int (line int))

(defgmethod
 (text-edit+get-line-wrap-index-at-column :class 'text-edit :bind
  "get_line_wrap_index_at_column" :hash 3175239445)
 int (line int) (column int))

(defgmethod
 (text-edit+get-line-wrapped-text :class 'text-edit :bind
  "get_line_wrapped_text" :hash 647634434)
 packed-string-array (line int))

(defgmethod
 (text-edit+set-smooth-scroll-enabled :class 'text-edit :bind
  "set_smooth_scroll_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-smooth-scroll-enabled :class 'text-edit :bind
  "is_smooth_scroll_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+get-v-scroll-bar :class 'text-edit :bind "get_v_scroll_bar" :hash
  3226026593)
 vscroll-bar)

(defgmethod
 (text-edit+get-h-scroll-bar :class 'text-edit :bind "get_h_scroll_bar" :hash
  3774687988)
 hscroll-bar)

(defgmethod
 (text-edit+set-v-scroll :class 'text-edit :bind "set_v_scroll" :hash
  373806689)
 :void (value float))

(defgmethod
 (text-edit+get-v-scroll :class 'text-edit :bind "get_v_scroll" :hash
  1740695150)
 float)

(defgmethod
 (text-edit+set-h-scroll :class 'text-edit :bind "set_h_scroll" :hash
  1286410249)
 :void (value int))

(defgmethod
 (text-edit+get-h-scroll :class 'text-edit :bind "get_h_scroll" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+set-scroll-past-end-of-file-enabled :class 'text-edit :bind
  "set_scroll_past_end_of_file_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (text-edit+is-scroll-past-end-of-file-enabled :class 'text-edit :bind
  "is_scroll_past_end_of_file_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-v-scroll-speed :class 'text-edit :bind "set_v_scroll_speed"
  :hash 373806689)
 :void (speed float))

(defgmethod
 (text-edit+get-v-scroll-speed :class 'text-edit :bind "get_v_scroll_speed"
  :hash 1740695150)
 float)

(defgmethod
 (text-edit+set-fit-content-height-enabled :class 'text-edit :bind
  "set_fit_content_height_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-fit-content-height-enabled :class 'text-edit :bind
  "is_fit_content_height_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-fit-content-width-enabled :class 'text-edit :bind
  "set_fit_content_width_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-fit-content-width-enabled :class 'text-edit :bind
  "is_fit_content_width_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+get-scroll-pos-for-line :class 'text-edit :bind
  "get_scroll_pos_for_line" :hash 3929084198)
 float (line int) (wrap-index int))

(defgmethod
 (text-edit+set-line-as-first-visible :class 'text-edit :bind
  "set_line_as_first_visible" :hash 2230941749)
 :void (line int) (wrap-index int))

(defgmethod
 (text-edit+get-first-visible-line :class 'text-edit :bind
  "get_first_visible_line" :hash 3905245786)
 int)

(defgmethod
 (text-edit+is-line-in-viewport :class 'text-edit :bind "is_line_in_viewport"
  :hash 1116898809)
 bool (line int))

(defgmethod
 (text-edit+set-line-as-center-visible :class 'text-edit :bind
  "set_line_as_center_visible" :hash 2230941749)
 :void (line int) (wrap-index int))

(defgmethod
 (text-edit+set-line-as-last-visible :class 'text-edit :bind
  "set_line_as_last_visible" :hash 2230941749)
 :void (line int) (wrap-index int))

(defgmethod
 (text-edit+get-last-full-visible-line :class 'text-edit :bind
  "get_last_full_visible_line" :hash 3905245786)
 int)

(defgmethod
 (text-edit+get-last-full-visible-line-wrap-index :class 'text-edit :bind
  "get_last_full_visible_line_wrap_index" :hash 3905245786)
 int)

(defgmethod
 (text-edit+get-visible-line-count :class 'text-edit :bind
  "get_visible_line_count" :hash 3905245786)
 int)

(defgmethod
 (text-edit+get-visible-line-count-in-range :class 'text-edit :bind
  "get_visible_line_count_in_range" :hash 3175239445)
 int (from-line int) (to-line int))

(defgmethod
 (text-edit+get-total-visible-line-count :class 'text-edit :bind
  "get_total_visible_line_count" :hash 3905245786)
 int)

(defgmethod
 (text-edit+adjust-viewport-to-caret :class 'text-edit :bind
  "adjust_viewport_to_caret" :hash 1995695955)
 :void (caret-index int))

(defgmethod
 (text-edit+center-viewport-to-caret :class 'text-edit :bind
  "center_viewport_to_caret" :hash 1995695955)
 :void (caret-index int))

(defgmethod
 (text-edit+set-draw-minimap :class 'text-edit :bind "set_draw_minimap" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-drawing-minimap :class 'text-edit :bind "is_drawing_minimap"
  :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-minimap-width :class 'text-edit :bind "set_minimap_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (text-edit+get-minimap-width :class 'text-edit :bind "get_minimap_width" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+get-minimap-visible-lines :class 'text-edit :bind
  "get_minimap_visible_lines" :hash 3905245786)
 int)

(defgmethod
 (text-edit+add-gutter :class 'text-edit :bind "add_gutter" :hash 1025054187)
 :void (at int))

(defgmethod
 (text-edit+remove-gutter :class 'text-edit :bind "remove_gutter" :hash
  1286410249)
 :void (gutter int))

(defgmethod
 (text-edit+get-gutter-count :class 'text-edit :bind "get_gutter_count" :hash
  3905245786)
 int)

(defgmethod
 (text-edit+set-gutter-name :class 'text-edit :bind "set_gutter_name" :hash
  501894301)
 :void (gutter int) (name string))

(defgmethod
 (text-edit+get-gutter-name :class 'text-edit :bind "get_gutter_name" :hash
  844755477)
 string (gutter int))

(defgmethod
 (text-edit+set-gutter-type :class 'text-edit :bind "set_gutter_type" :hash
  1088959071)
 :void (gutter int) (type text-edit+gutter-type))

(defgmethod
 (text-edit+get-gutter-type :class 'text-edit :bind "get_gutter_type" :hash
  1159699127)
 text-edit+gutter-type (gutter int))

(defgmethod
 (text-edit+set-gutter-width :class 'text-edit :bind "set_gutter_width" :hash
  3937882851)
 :void (gutter int) (width int))

(defgmethod
 (text-edit+get-gutter-width :class 'text-edit :bind "get_gutter_width" :hash
  923996154)
 int (gutter int))

(defgmethod
 (text-edit+set-gutter-draw :class 'text-edit :bind "set_gutter_draw" :hash
  300928843)
 :void (gutter int) (draw bool))

(defgmethod
 (text-edit+is-gutter-drawn :class 'text-edit :bind "is_gutter_drawn" :hash
  1116898809)
 bool (gutter int))

(defgmethod
 (text-edit+set-gutter-clickable :class 'text-edit :bind "set_gutter_clickable"
  :hash 300928843)
 :void (gutter int) (clickable bool))

(defgmethod
 (text-edit+is-gutter-clickable :class 'text-edit :bind "is_gutter_clickable"
  :hash 1116898809)
 bool (gutter int))

(defgmethod
 (text-edit+set-gutter-overwritable :class 'text-edit :bind
  "set_gutter_overwritable" :hash 300928843)
 :void (gutter int) (overwritable bool))

(defgmethod
 (text-edit+is-gutter-overwritable :class 'text-edit :bind
  "is_gutter_overwritable" :hash 1116898809)
 bool (gutter int))

(defgmethod
 (text-edit+merge-gutters :class 'text-edit :bind "merge_gutters" :hash
  3937882851)
 :void (from-line int) (to-line int))

(defgmethod
 (text-edit+set-gutter-custom-draw :class 'text-edit :bind
  "set_gutter_custom_draw" :hash 957362965)
 :void (column int) (draw-callback callable))

(defgmethod
 (text-edit+get-total-gutter-width :class 'text-edit :bind
  "get_total_gutter_width" :hash 3905245786)
 int)

(defgmethod
 (text-edit+set-line-gutter-metadata :class 'text-edit :bind
  "set_line_gutter_metadata" :hash 2060538656)
 :void (line int) (gutter int) (metadata variant))

(defgmethod
 (text-edit+get-line-gutter-metadata :class 'text-edit :bind
  "get_line_gutter_metadata" :hash 678354945)
 variant (line int) (gutter int))

(defgmethod
 (text-edit+set-line-gutter-text :class 'text-edit :bind "set_line_gutter_text"
  :hash 2285447957)
 :void (line int) (gutter int) (text string))

(defgmethod
 (text-edit+get-line-gutter-text :class 'text-edit :bind "get_line_gutter_text"
  :hash 1391810591)
 string (line int) (gutter int))

(defgmethod
 (text-edit+set-line-gutter-icon :class 'text-edit :bind "set_line_gutter_icon"
  :hash 176101966)
 :void (line int) (gutter int) (icon texture-2d))

(defgmethod
 (text-edit+get-line-gutter-icon :class 'text-edit :bind "get_line_gutter_icon"
  :hash 2584904275)
 texture-2d (line int) (gutter int))

(defgmethod
 (text-edit+set-line-gutter-item-color :class 'text-edit :bind
  "set_line_gutter_item_color" :hash 3733378741)
 :void (line int) (gutter int) (color color))

(defgmethod
 (text-edit+get-line-gutter-item-color :class 'text-edit :bind
  "get_line_gutter_item_color" :hash 2165839948)
 color (line int) (gutter int))

(defgmethod
 (text-edit+set-line-gutter-clickable :class 'text-edit :bind
  "set_line_gutter_clickable" :hash 1383440665)
 :void (line int) (gutter int) (clickable bool))

(defgmethod
 (text-edit+is-line-gutter-clickable :class 'text-edit :bind
  "is_line_gutter_clickable" :hash 2522259332)
 bool (line int) (gutter int))

(defgmethod
 (text-edit+set-line-background-color :class 'text-edit :bind
  "set_line_background_color" :hash 2878471219)
 :void (line int) (color color))

(defgmethod
 (text-edit+get-line-background-color :class 'text-edit :bind
  "get_line_background_color" :hash 3457211756)
 color (line int))

(defgmethod
 (text-edit+set-syntax-highlighter :class 'text-edit :bind
  "set_syntax_highlighter" :hash 2765644541)
 :void (syntax-highlighter syntax-highlighter))

(defgmethod
 (text-edit+get-syntax-highlighter :class 'text-edit :bind
  "get_syntax_highlighter" :hash 2721131626)
 syntax-highlighter)

(defgmethod
 (text-edit+set-highlight-current-line :class 'text-edit :bind
  "set_highlight_current_line" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-highlight-current-line-enabled :class 'text-edit :bind
  "is_highlight_current_line_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-highlight-all-occurrences :class 'text-edit :bind
  "set_highlight_all_occurrences" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-highlight-all-occurrences-enabled :class 'text-edit :bind
  "is_highlight_all_occurrences_enabled" :hash 36873697)
 bool)

(defgmethod
 (text-edit+get-draw-control-chars :class 'text-edit :bind
  "get_draw_control_chars" :hash 36873697)
 bool)

(defgmethod
 (text-edit+set-draw-control-chars :class 'text-edit :bind
  "set_draw_control_chars" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+set-draw-tabs :class 'text-edit :bind "set_draw_tabs" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-drawing-tabs :class 'text-edit :bind "is_drawing_tabs" :hash
  36873697)
 bool)

(defgmethod
 (text-edit+set-draw-spaces :class 'text-edit :bind "set_draw_spaces" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (text-edit+is-drawing-spaces :class 'text-edit :bind "is_drawing_spaces" :hash
  36873697)
 bool)

(defgmethod
 (text-edit+get-menu :class 'text-edit :bind "get_menu" :hash 229722558)
 popup-menu)

(defgmethod
 (text-edit+is-menu-visible :class 'text-edit :bind "is_menu_visible" :hash
  36873697)
 bool)

(defgmethod
 (text-edit+menu-option :class 'text-edit :bind "menu_option" :hash 1286410249)
 :void (option int))

(defgmethod
 (text-edit+adjust-carets-after-edit :class 'text-edit :bind
  "adjust_carets_after_edit" :hash 1770277138)
 :void (caret int) (from-line int) (from-col int) (to-line int) (to-col int))

(defgmethod
 (text-edit+get-caret-index-edit-order :class 'text-edit :bind
  "get_caret_index_edit_order" :hash 969006518)
 packed-int-32array)

(defgmethod
 (text-edit+get-selection-line :class 'text-edit :bind "get_selection_line"
  :hash 1591665591)
 int (caret-index int))

(defgmethod
 (text-edit+get-selection-column :class 'text-edit :bind "get_selection_column"
  :hash 1591665591)
 int (caret-index int))