(common-lisp:in-package :%godot)


(defgmethod
 (rich-text-label+get-parsed-text :class 'rich-text-label :bind
  "get_parsed_text" :hash 201670096)
 string)

(defgmethod
 (rich-text-label+add-text :class 'rich-text-label :bind "add_text" :hash
  83702148)
 :void (text string))

(defgmethod
 (rich-text-label+set-text :class 'rich-text-label :bind "set_text" :hash
  83702148)
 :void (text string))

(defgmethod
 (rich-text-label+add-hr :class 'rich-text-label :bind "add_hr" :hash 16816895)
 :void (width int) (height int) (color color) (alignment horizontal-alignment)
 (width-in-percent bool) (height-in-percent bool))

(defgmethod
 (rich-text-label+add-image :class 'rich-text-label :bind "add_image" :hash
  1390915033)
 :void (image texture-2d) (width int) (height int) (color color)
 (inline-align inline-alignment) (region rect-2) (key variant) (pad bool)
 (tooltip string) (width-in-percent bool) (height-in-percent bool)
 (alt-text string))

(defgmethod
 (rich-text-label+update-image :class 'rich-text-label :bind "update_image"
  :hash 6389170)
 :void (key variant) (mask rich-text-label+image-update-mask)
 (image texture-2d) (width int) (height int) (color color)
 (inline-align inline-alignment) (region rect-2) (pad bool) (tooltip string)
 (width-in-percent bool) (height-in-percent bool))

(defgmethod
 (rich-text-label+newline :class 'rich-text-label :bind "newline" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+remove-paragraph :class 'rich-text-label :bind
  "remove_paragraph" :hash 3262369265)
 bool (paragraph int) (no-invalidate bool))

(defgmethod
 (rich-text-label+invalidate-paragraph :class 'rich-text-label :bind
  "invalidate_paragraph" :hash 3067735520)
 bool (paragraph int))

(defgmethod
 (rich-text-label+push-font :class 'rich-text-label :bind "push_font" :hash
  2347424842)
 :void (font font) (font-size int))

(defgmethod
 (rich-text-label+push-font-size :class 'rich-text-label :bind "push_font_size"
  :hash 1286410249)
 :void (font-size int))

(defgmethod
 (rich-text-label+push-normal :class 'rich-text-label :bind "push_normal" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+push-bold :class 'rich-text-label :bind "push_bold" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+push-bold-italics :class 'rich-text-label :bind
  "push_bold_italics" :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+push-italics :class 'rich-text-label :bind "push_italics"
  :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+push-mono :class 'rich-text-label :bind "push_mono" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+push-color :class 'rich-text-label :bind "push_color" :hash
  2920490490)
 :void (color color))

(defgmethod
 (rich-text-label+push-outline-size :class 'rich-text-label :bind
  "push_outline_size" :hash 1286410249)
 :void (outline-size int))

(defgmethod
 (rich-text-label+push-outline-color :class 'rich-text-label :bind
  "push_outline_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (rich-text-label+push-paragraph :class 'rich-text-label :bind "push_paragraph"
  :hash 3089306873)
 :void (alignment horizontal-alignment) (base-direction control+text-direction)
 (language string) (st-parser text-server+structured-text-parser)
 (justification-flags text-server+justification-flag)
 (tab-stops packed-float-32array))

(defgmethod
 (rich-text-label+push-indent :class 'rich-text-label :bind "push_indent" :hash
  1286410249)
 :void (level int))

(defgmethod
 (rich-text-label+push-list :class 'rich-text-label :bind "push_list" :hash
  3017143144)
 :void (level int) (type rich-text-label+list-type) (capitalize bool)
 (bullet string))

(defgmethod
 (rich-text-label+push-meta :class 'rich-text-label :bind "push_meta" :hash
  3765356747)
 :void (data variant) (underline-mode rich-text-label+meta-underline)
 (tooltip string))

(defgmethod
 (rich-text-label+push-hint :class 'rich-text-label :bind "push_hint" :hash
  83702148)
 :void (description string))

(defgmethod
 (rich-text-label+push-language :class 'rich-text-label :bind "push_language"
  :hash 83702148)
 :void (language string))

(defgmethod
 (rich-text-label+push-underline :class 'rich-text-label :bind "push_underline"
  :hash 1458098034)
 :void (color color))

(defgmethod
 (rich-text-label+push-strikethrough :class 'rich-text-label :bind
  "push_strikethrough" :hash 1458098034)
 :void (color color))

(defgmethod
 (rich-text-label+push-table :class 'rich-text-label :bind "push_table" :hash
  3426862026)
 :void (columns int) (inline-align inline-alignment) (align-to-row int)
 (name string))

(defgmethod
 (rich-text-label+push-dropcap :class 'rich-text-label :bind "push_dropcap"
  :hash 4061635501)
 :void (string string) (font font) (size int) (dropcap-margins rect-2)
 (color color) (outline-size int) (outline-color color))

(defgmethod
 (rich-text-label+set-table-column-expand :class 'rich-text-label :bind
  "set_table_column_expand" :hash 117236061)
 :void (column int) (expand bool) (ratio int) (shrink bool))

(defgmethod
 (rich-text-label+set-table-column-name :class 'rich-text-label :bind
  "set_table_column_name" :hash 501894301)
 :void (column int) (name string))

(defgmethod
 (rich-text-label+set-cell-row-background-color :class 'rich-text-label :bind
  "set_cell_row_background_color" :hash 3465483165)
 :void (odd-row-bg color) (even-row-bg color))

(defgmethod
 (rich-text-label+set-cell-border-color :class 'rich-text-label :bind
  "set_cell_border_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (rich-text-label+set-cell-size-override :class 'rich-text-label :bind
  "set_cell_size_override" :hash 3108078480)
 :void (min-size vector-2) (max-size vector-2))

(defgmethod
 (rich-text-label+set-cell-padding :class 'rich-text-label :bind
  "set_cell_padding" :hash 2046264180)
 :void (padding rect-2))

(defgmethod
 (rich-text-label+push-cell :class 'rich-text-label :bind "push_cell" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+push-fgcolor :class 'rich-text-label :bind "push_fgcolor"
  :hash 2920490490)
 :void (fgcolor color))

(defgmethod
 (rich-text-label+push-bgcolor :class 'rich-text-label :bind "push_bgcolor"
  :hash 2920490490)
 :void (bgcolor color))

(defgmethod
 (rich-text-label+push-customfx :class 'rich-text-label :bind "push_customfx"
  :hash 2337942958)
 :void (effect rich-text-effect) (env dictionary))

(defgmethod
 (rich-text-label+push-context :class 'rich-text-label :bind "push_context"
  :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+pop-context :class 'rich-text-label :bind "pop_context" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+pop :class 'rich-text-label :bind "pop" :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+pop-all :class 'rich-text-label :bind "pop_all" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+clear :class 'rich-text-label :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+set-structured-text-bidi-override :class 'rich-text-label
  :bind "set_structured_text_bidi_override" :hash 55961453)
 :void (parser text-server+structured-text-parser))

(defgmethod
 (rich-text-label+get-structured-text-bidi-override :class 'rich-text-label
  :bind "get_structured_text_bidi_override" :hash 3385126229)
 text-server+structured-text-parser)

(defgmethod
 (rich-text-label+set-structured-text-bidi-override-options :class
  'rich-text-label :bind "set_structured_text_bidi_override_options" :hash
  381264803)
 :void (args array))

(defgmethod
 (rich-text-label+get-structured-text-bidi-override-options :class
  'rich-text-label :bind "get_structured_text_bidi_override_options" :hash
  3995934104)
 array)

(defgmethod
 (rich-text-label+set-text-direction :class 'rich-text-label :bind
  "set_text_direction" :hash 119160795)
 :void (direction control+text-direction))

(defgmethod
 (rich-text-label+get-text-direction :class 'rich-text-label :bind
  "get_text_direction" :hash 797257663)
 control+text-direction)

(defgmethod
 (rich-text-label+set-language :class 'rich-text-label :bind "set_language"
  :hash 83702148)
 :void (language string))

(defgmethod
 (rich-text-label+get-language :class 'rich-text-label :bind "get_language"
  :hash 201670096)
 string)

(defgmethod
 (rich-text-label+set-horizontal-alignment :class 'rich-text-label :bind
  "set_horizontal_alignment" :hash 2312603777)
 :void (alignment horizontal-alignment))

(defgmethod
 (rich-text-label+get-horizontal-alignment :class 'rich-text-label :bind
  "get_horizontal_alignment" :hash 341400642)
 horizontal-alignment)

(defgmethod
 (rich-text-label+set-vertical-alignment :class 'rich-text-label :bind
  "set_vertical_alignment" :hash 1796458609)
 :void (alignment vertical-alignment))

(defgmethod
 (rich-text-label+get-vertical-alignment :class 'rich-text-label :bind
  "get_vertical_alignment" :hash 3274884059)
 vertical-alignment)

(defgmethod
 (rich-text-label+set-justification-flags :class 'rich-text-label :bind
  "set_justification_flags" :hash 2877345813)
 :void (justification-flags text-server+justification-flag))

(defgmethod
 (rich-text-label+get-justification-flags :class 'rich-text-label :bind
  "get_justification_flags" :hash 1583363614)
 text-server+justification-flag)

(defgmethod
 (rich-text-label+set-tab-stops :class 'rich-text-label :bind "set_tab_stops"
  :hash 2899603908)
 :void (tab-stops packed-float-32array))

(defgmethod
 (rich-text-label+get-tab-stops :class 'rich-text-label :bind "get_tab_stops"
  :hash 675695659)
 packed-float-32array)

(defgmethod
 (rich-text-label+set-autowrap-mode :class 'rich-text-label :bind
  "set_autowrap_mode" :hash 3289138044)
 :void (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (rich-text-label+get-autowrap-mode :class 'rich-text-label :bind
  "get_autowrap_mode" :hash 1549071663)
 text-server+autowrap-mode)

(defgmethod
 (rich-text-label+set-autowrap-trim-flags :class 'rich-text-label :bind
  "set_autowrap_trim_flags" :hash 2809697122)
 :void (autowrap-trim-flags text-server+line-break-flag))

(defgmethod
 (rich-text-label+get-autowrap-trim-flags :class 'rich-text-label :bind
  "get_autowrap_trim_flags" :hash 2340632602)
 text-server+line-break-flag)

(defgmethod
 (rich-text-label+set-meta-underline :class 'rich-text-label :bind
  "set_meta_underline" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rich-text-label+is-meta-underlined :class 'rich-text-label :bind
  "is_meta_underlined" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-hint-underline :class 'rich-text-label :bind
  "set_hint_underline" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rich-text-label+is-hint-underlined :class 'rich-text-label :bind
  "is_hint_underlined" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-scroll-active :class 'rich-text-label :bind
  "set_scroll_active" :hash 2586408642)
 :void (active bool))

(defgmethod
 (rich-text-label+is-scroll-active :class 'rich-text-label :bind
  "is_scroll_active" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-scroll-follow-visible-characters :class 'rich-text-label
  :bind "set_scroll_follow_visible_characters" :hash 2586408642)
 :void (follow bool))

(defgmethod
 (rich-text-label+is-scroll-following-visible-characters :class
  'rich-text-label :bind "is_scroll_following_visible_characters" :hash
  36873697)
 bool)

(defgmethod
 (rich-text-label+set-scroll-follow :class 'rich-text-label :bind
  "set_scroll_follow" :hash 2586408642)
 :void (follow bool))

(defgmethod
 (rich-text-label+is-scroll-following :class 'rich-text-label :bind
  "is_scroll_following" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+get-v-scroll-bar :class 'rich-text-label :bind
  "get_v_scroll_bar" :hash 2630340773)
 vscroll-bar)

(defgmethod
 (rich-text-label+scroll-to-line :class 'rich-text-label :bind "scroll_to_line"
  :hash 1286410249)
 :void (line int))

(defgmethod
 (rich-text-label+scroll-to-paragraph :class 'rich-text-label :bind
  "scroll_to_paragraph" :hash 1286410249)
 :void (paragraph int))

(defgmethod
 (rich-text-label+scroll-to-selection :class 'rich-text-label :bind
  "scroll_to_selection" :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+set-tab-size :class 'rich-text-label :bind "set_tab_size"
  :hash 1286410249)
 :void (spaces int))

(defgmethod
 (rich-text-label+get-tab-size :class 'rich-text-label :bind "get_tab_size"
  :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+set-fit-content :class 'rich-text-label :bind
  "set_fit_content" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rich-text-label+is-fit-content-enabled :class 'rich-text-label :bind
  "is_fit_content_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-selection-enabled :class 'rich-text-label :bind
  "set_selection_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rich-text-label+is-selection-enabled :class 'rich-text-label :bind
  "is_selection_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-context-menu-enabled :class 'rich-text-label :bind
  "set_context_menu_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rich-text-label+is-context-menu-enabled :class 'rich-text-label :bind
  "is_context_menu_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-shortcut-keys-enabled :class 'rich-text-label :bind
  "set_shortcut_keys_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (rich-text-label+is-shortcut-keys-enabled :class 'rich-text-label :bind
  "is_shortcut_keys_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-deselect-on-focus-loss-enabled :class 'rich-text-label
  :bind "set_deselect_on_focus_loss_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rich-text-label+is-deselect-on-focus-loss-enabled :class 'rich-text-label
  :bind "is_deselect_on_focus_loss_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+set-drag-and-drop-selection-enabled :class 'rich-text-label
  :bind "set_drag_and_drop_selection_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rich-text-label+is-drag-and-drop-selection-enabled :class 'rich-text-label
  :bind "is_drag_and_drop_selection_enabled" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+get-selection-from :class 'rich-text-label :bind
  "get_selection_from" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-selection-to :class 'rich-text-label :bind
  "get_selection_to" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-selection-line-offset :class 'rich-text-label :bind
  "get_selection_line_offset" :hash 1740695150)
 float)

(defgmethod
 (rich-text-label+select-all :class 'rich-text-label :bind "select_all" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+get-selected-text :class 'rich-text-label :bind
  "get_selected_text" :hash 201670096)
 string)

(defgmethod
 (rich-text-label+deselect :class 'rich-text-label :bind "deselect" :hash
  3218959716)
 :void)

(defgmethod
 (rich-text-label+parse-bbcode :class 'rich-text-label :bind "parse_bbcode"
  :hash 83702148)
 :void (bbcode string))

(defgmethod
 (rich-text-label+append-text :class 'rich-text-label :bind "append_text" :hash
  83702148)
 :void (bbcode string))

(defgmethod
 (rich-text-label+get-text :class 'rich-text-label :bind "get_text" :hash
  201670096)
 string)

(defgmethod
 (rich-text-label+is-ready :class 'rich-text-label :bind "is_ready" :hash
  36873697)
 bool)

(defgmethod
 (rich-text-label+is-finished :class 'rich-text-label :bind "is_finished" :hash
  36873697)
 bool)

(defgmethod
 (rich-text-label+set-threaded :class 'rich-text-label :bind "set_threaded"
  :hash 2586408642)
 :void (threaded bool))

(defgmethod
 (rich-text-label+is-threaded :class 'rich-text-label :bind "is_threaded" :hash
  36873697)
 bool)

(defgmethod
 (rich-text-label+set-progress-bar-delay :class 'rich-text-label :bind
  "set_progress_bar_delay" :hash 1286410249)
 :void (delay-ms int))

(defgmethod
 (rich-text-label+get-progress-bar-delay :class 'rich-text-label :bind
  "get_progress_bar_delay" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+set-visible-characters :class 'rich-text-label :bind
  "set_visible_characters" :hash 1286410249)
 :void (amount int))

(defgmethod
 (rich-text-label+get-visible-characters :class 'rich-text-label :bind
  "get_visible_characters" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-visible-characters-behavior :class 'rich-text-label :bind
  "get_visible_characters_behavior" :hash 258789322)
 text-server+visible-characters-behavior)

(defgmethod
 (rich-text-label+set-visible-characters-behavior :class 'rich-text-label :bind
  "set_visible_characters_behavior" :hash 3383839701)
 :void (behavior text-server+visible-characters-behavior))

(defgmethod
 (rich-text-label+set-visible-ratio :class 'rich-text-label :bind
  "set_visible_ratio" :hash 373806689)
 :void (ratio float))

(defgmethod
 (rich-text-label+get-visible-ratio :class 'rich-text-label :bind
  "get_visible_ratio" :hash 1740695150)
 float)

(defgmethod
 (rich-text-label+get-character-line :class 'rich-text-label :bind
  "get_character_line" :hash 3744713108)
 int (character int))

(defgmethod
 (rich-text-label+get-character-paragraph :class 'rich-text-label :bind
  "get_character_paragraph" :hash 3744713108)
 int (character int))

(defgmethod
 (rich-text-label+get-total-character-count :class 'rich-text-label :bind
  "get_total_character_count" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+set-use-bbcode :class 'rich-text-label :bind "set_use_bbcode"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (rich-text-label+is-using-bbcode :class 'rich-text-label :bind
  "is_using_bbcode" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+get-line-count :class 'rich-text-label :bind "get_line_count"
  :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-line-range :class 'rich-text-label :bind "get_line_range"
  :hash 3665014314)
 vector-2i (line int))

(defgmethod
 (rich-text-label+get-visible-line-count :class 'rich-text-label :bind
  "get_visible_line_count" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-paragraph-count :class 'rich-text-label :bind
  "get_paragraph_count" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-visible-paragraph-count :class 'rich-text-label :bind
  "get_visible_paragraph_count" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-content-height :class 'rich-text-label :bind
  "get_content_height" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-content-width :class 'rich-text-label :bind
  "get_content_width" :hash 3905245786)
 int)

(defgmethod
 (rich-text-label+get-line-height :class 'rich-text-label :bind
  "get_line_height" :hash 923996154)
 int (line int))

(defgmethod
 (rich-text-label+get-line-width :class 'rich-text-label :bind "get_line_width"
  :hash 923996154)
 int (line int))

(defgmethod
 (rich-text-label+get-visible-content-rect :class 'rich-text-label :bind
  "get_visible_content_rect" :hash 410525958)
 rect-2i)

(defgmethod
 (rich-text-label+get-line-offset :class 'rich-text-label :bind
  "get_line_offset" :hash 4025615559)
 float (line int))

(defgmethod
 (rich-text-label+get-paragraph-offset :class 'rich-text-label :bind
  "get_paragraph_offset" :hash 4025615559)
 float (paragraph int))

(defgmethod
 (rich-text-label+parse-expressions-for-values :class 'rich-text-label :bind
  "parse_expressions_for_values" :hash 1522900837)
 dictionary (expressions packed-string-array))

(defgmethod
 (rich-text-label+set-effects :class 'rich-text-label :bind "set_effects" :hash
  381264803)
 :void (effects array))

(defgmethod
 (rich-text-label+get-effects :class 'rich-text-label :bind "get_effects" :hash
  2915620761)
 array)

(defgmethod
 (rich-text-label+install-effect :class 'rich-text-label :bind "install_effect"
  :hash 1114965689)
 :void (effect variant))

(defgmethod
 (rich-text-label+reload-effects :class 'rich-text-label :bind "reload_effects"
  :hash 3218959716)
 :void)

(defgmethod
 (rich-text-label+get-menu :class 'rich-text-label :bind "get_menu" :hash
  229722558)
 popup-menu)

(defgmethod
 (rich-text-label+is-menu-visible :class 'rich-text-label :bind
  "is_menu_visible" :hash 36873697)
 bool)

(defgmethod
 (rich-text-label+menu-option :class 'rich-text-label :bind "menu_option" :hash
  1286410249)
 :void (option int))