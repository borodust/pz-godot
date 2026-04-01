(common-lisp:in-package :%godot)


(defgmethod
 (code-edit+%confirm-code-completion :class 'code-edit :bind
  "_confirm_code_completion" :hash 2586408642 :virtual common-lisp:t)
 :void (replace bool))

(defgmethod
 (code-edit+%request-code-completion :class 'code-edit :bind
  "_request_code_completion" :hash 2586408642 :virtual common-lisp:t)
 :void (force bool))

(defgmethod
 (code-edit+%filter-code-completion-candidates :class 'code-edit :bind
  "_filter_code_completion_candidates" :hash 2560709669 :virtual common-lisp:t)
 array (candidates array))

(defgmethod
 (code-edit+set-indent-size :class 'code-edit :bind "set_indent_size" :hash
  1286410249)
 :void (size int))

(defgmethod
 (code-edit+get-indent-size :class 'code-edit :bind "get_indent_size" :hash
  3905245786)
 int)

(defgmethod
 (code-edit+set-indent-using-spaces :class 'code-edit :bind
  "set_indent_using_spaces" :hash 2586408642)
 :void (use-spaces bool))

(defgmethod
 (code-edit+is-indent-using-spaces :class 'code-edit :bind
  "is_indent_using_spaces" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-auto-indent-enabled :class 'code-edit :bind
  "set_auto_indent_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-auto-indent-enabled :class 'code-edit :bind
  "is_auto_indent_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-auto-indent-prefixes :class 'code-edit :bind
  "set_auto_indent_prefixes" :hash 381264803)
 :void (prefixes array))

(defgmethod
 (code-edit+get-auto-indent-prefixes :class 'code-edit :bind
  "get_auto_indent_prefixes" :hash 3995934104)
 array)

(defgmethod
 (code-edit+do-indent :class 'code-edit :bind "do_indent" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+indent-lines :class 'code-edit :bind "indent_lines" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+unindent-lines :class 'code-edit :bind "unindent_lines" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+convert-indent :class 'code-edit :bind "convert_indent" :hash
  423910286)
 :void (from-line int) (to-line int))

(defgmethod
 (code-edit+set-auto-brace-completion-enabled :class 'code-edit :bind
  "set_auto_brace_completion_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-auto-brace-completion-enabled :class 'code-edit :bind
  "is_auto_brace_completion_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-highlight-matching-braces-enabled :class 'code-edit :bind
  "set_highlight_matching_braces_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-highlight-matching-braces-enabled :class 'code-edit :bind
  "is_highlight_matching_braces_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+add-auto-brace-completion-pair :class 'code-edit :bind
  "add_auto_brace_completion_pair" :hash 3186203200)
 :void (start-key string) (end-key string))

(defgmethod
 (code-edit+set-auto-brace-completion-pairs :class 'code-edit :bind
  "set_auto_brace_completion_pairs" :hash 4155329257)
 :void (pairs dictionary))

(defgmethod
 (code-edit+get-auto-brace-completion-pairs :class 'code-edit :bind
  "get_auto_brace_completion_pairs" :hash 3102165223)
 dictionary)

(defgmethod
 (code-edit+has-auto-brace-completion-open-key :class 'code-edit :bind
  "has_auto_brace_completion_open_key" :hash 3927539163)
 bool (open-key string))

(defgmethod
 (code-edit+has-auto-brace-completion-close-key :class 'code-edit :bind
  "has_auto_brace_completion_close_key" :hash 3927539163)
 bool (close-key string))

(defgmethod
 (code-edit+get-auto-brace-completion-close-key :class 'code-edit :bind
  "get_auto_brace_completion_close_key" :hash 3135753539)
 string (open-key string))

(defgmethod
 (code-edit+set-draw-breakpoints-gutter :class 'code-edit :bind
  "set_draw_breakpoints_gutter" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-drawing-breakpoints-gutter :class 'code-edit :bind
  "is_drawing_breakpoints_gutter" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-draw-bookmarks-gutter :class 'code-edit :bind
  "set_draw_bookmarks_gutter" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-drawing-bookmarks-gutter :class 'code-edit :bind
  "is_drawing_bookmarks_gutter" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-draw-executing-lines-gutter :class 'code-edit :bind
  "set_draw_executing_lines_gutter" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-drawing-executing-lines-gutter :class 'code-edit :bind
  "is_drawing_executing_lines_gutter" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-line-as-breakpoint :class 'code-edit :bind
  "set_line_as_breakpoint" :hash 300928843)
 :void (line int) (breakpointed bool))

(defgmethod
 (code-edit+is-line-breakpointed :class 'code-edit :bind "is_line_breakpointed"
  :hash 1116898809)
 bool (line int))

(defgmethod
 (code-edit+clear-breakpointed-lines :class 'code-edit :bind
  "clear_breakpointed_lines" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-breakpointed-lines :class 'code-edit :bind
  "get_breakpointed_lines" :hash 1930428628)
 packed-int-32array)

(defgmethod
 (code-edit+set-line-as-bookmarked :class 'code-edit :bind
  "set_line_as_bookmarked" :hash 300928843)
 :void (line int) (bookmarked bool))

(defgmethod
 (code-edit+is-line-bookmarked :class 'code-edit :bind "is_line_bookmarked"
  :hash 1116898809)
 bool (line int))

(defgmethod
 (code-edit+clear-bookmarked-lines :class 'code-edit :bind
  "clear_bookmarked_lines" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-bookmarked-lines :class 'code-edit :bind "get_bookmarked_lines"
  :hash 1930428628)
 packed-int-32array)

(defgmethod
 (code-edit+set-line-as-executing :class 'code-edit :bind
  "set_line_as_executing" :hash 300928843)
 :void (line int) (executing bool))

(defgmethod
 (code-edit+is-line-executing :class 'code-edit :bind "is_line_executing" :hash
  1116898809)
 bool (line int))

(defgmethod
 (code-edit+clear-executing-lines :class 'code-edit :bind
  "clear_executing_lines" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-executing-lines :class 'code-edit :bind "get_executing_lines"
  :hash 1930428628)
 packed-int-32array)

(defgmethod
 (code-edit+set-draw-line-numbers :class 'code-edit :bind
  "set_draw_line_numbers" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-draw-line-numbers-enabled :class 'code-edit :bind
  "is_draw_line_numbers_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-line-numbers-zero-padded :class 'code-edit :bind
  "set_line_numbers_zero_padded" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-line-numbers-zero-padded :class 'code-edit :bind
  "is_line_numbers_zero_padded" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-line-numbers-min-digits :class 'code-edit :bind
  "set_line_numbers_min_digits" :hash 1286410249)
 :void (count int))

(defgmethod
 (code-edit+get-line-numbers-min-digits :class 'code-edit :bind
  "get_line_numbers_min_digits" :hash 3905245786)
 int)

(defgmethod
 (code-edit+set-draw-fold-gutter :class 'code-edit :bind "set_draw_fold_gutter"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-drawing-fold-gutter :class 'code-edit :bind
  "is_drawing_fold_gutter" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-line-folding-enabled :class 'code-edit :bind
  "set_line_folding_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (code-edit+is-line-folding-enabled :class 'code-edit :bind
  "is_line_folding_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+can-fold-line :class 'code-edit :bind "can_fold_line" :hash
  1116898809)
 bool (line int))

(defgmethod
 (code-edit+fold-line :class 'code-edit :bind "fold_line" :hash 1286410249)
 :void (line int))

(defgmethod
 (code-edit+unfold-line :class 'code-edit :bind "unfold_line" :hash 1286410249)
 :void (line int))

(defgmethod
 (code-edit+fold-all-lines :class 'code-edit :bind "fold_all_lines" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+unfold-all-lines :class 'code-edit :bind "unfold_all_lines" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+toggle-foldable-line :class 'code-edit :bind "toggle_foldable_line"
  :hash 1286410249)
 :void (line int))

(defgmethod
 (code-edit+toggle-foldable-lines-at-carets :class 'code-edit :bind
  "toggle_foldable_lines_at_carets" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+is-line-folded :class 'code-edit :bind "is_line_folded" :hash
  1116898809)
 bool (line int))

(defgmethod
 (code-edit+get-folded-lines :class 'code-edit :bind "get_folded_lines" :hash
  3995934104)
 array)

(defgmethod
 (code-edit+create-code-region :class 'code-edit :bind "create_code_region"
  :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-code-region-start-tag :class 'code-edit :bind
  "get_code_region_start_tag" :hash 201670096)
 string)

(defgmethod
 (code-edit+get-code-region-end-tag :class 'code-edit :bind
  "get_code_region_end_tag" :hash 201670096)
 string)

(defgmethod
 (code-edit+set-code-region-tags :class 'code-edit :bind "set_code_region_tags"
  :hash 708800718)
 :void (start string) (end string))

(defgmethod
 (code-edit+is-line-code-region-start :class 'code-edit :bind
  "is_line_code_region_start" :hash 1116898809)
 bool (line int))

(defgmethod
 (code-edit+is-line-code-region-end :class 'code-edit :bind
  "is_line_code_region_end" :hash 1116898809)
 bool (line int))

(defgmethod
 (code-edit+add-string-delimiter :class 'code-edit :bind "add_string_delimiter"
  :hash 3146098955)
 :void (start-key string) (end-key string) (line-only bool))

(defgmethod
 (code-edit+remove-string-delimiter :class 'code-edit :bind
  "remove_string_delimiter" :hash 83702148)
 :void (start-key string))

(defgmethod
 (code-edit+has-string-delimiter :class 'code-edit :bind "has_string_delimiter"
  :hash 3927539163)
 bool (start-key string))

(defgmethod
 (code-edit+set-string-delimiters :class 'code-edit :bind
  "set_string_delimiters" :hash 381264803)
 :void (string-delimiters array))

(defgmethod
 (code-edit+clear-string-delimiters :class 'code-edit :bind
  "clear_string_delimiters" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-string-delimiters :class 'code-edit :bind
  "get_string_delimiters" :hash 3995934104)
 array)

(defgmethod
 (code-edit+is-in-string :class 'code-edit :bind "is_in_string" :hash
  688195400)
 int (line int) (column int))

(defgmethod
 (code-edit+add-comment-delimiter :class 'code-edit :bind
  "add_comment_delimiter" :hash 3146098955)
 :void (start-key string) (end-key string) (line-only bool))

(defgmethod
 (code-edit+remove-comment-delimiter :class 'code-edit :bind
  "remove_comment_delimiter" :hash 83702148)
 :void (start-key string))

(defgmethod
 (code-edit+has-comment-delimiter :class 'code-edit :bind
  "has_comment_delimiter" :hash 3927539163)
 bool (start-key string))

(defgmethod
 (code-edit+set-comment-delimiters :class 'code-edit :bind
  "set_comment_delimiters" :hash 381264803)
 :void (comment-delimiters array))

(defgmethod
 (code-edit+clear-comment-delimiters :class 'code-edit :bind
  "clear_comment_delimiters" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+get-comment-delimiters :class 'code-edit :bind
  "get_comment_delimiters" :hash 3995934104)
 array)

(defgmethod
 (code-edit+is-in-comment :class 'code-edit :bind "is_in_comment" :hash
  688195400)
 int (line int) (column int))

(defgmethod
 (code-edit+get-delimiter-start-key :class 'code-edit :bind
  "get_delimiter_start_key" :hash 844755477)
 string (delimiter-index int))

(defgmethod
 (code-edit+get-delimiter-end-key :class 'code-edit :bind
  "get_delimiter_end_key" :hash 844755477)
 string (delimiter-index int))

(defgmethod
 (code-edit+get-delimiter-start-position :class 'code-edit :bind
  "get_delimiter_start_position" :hash 3016396712)
 vector-2 (line int) (column int))

(defgmethod
 (code-edit+get-delimiter-end-position :class 'code-edit :bind
  "get_delimiter_end_position" :hash 3016396712)
 vector-2 (line int) (column int))

(defgmethod
 (code-edit+set-code-hint :class 'code-edit :bind "set_code_hint" :hash
  83702148)
 :void (code-hint string))

(defgmethod
 (code-edit+set-code-hint-draw-below :class 'code-edit :bind
  "set_code_hint_draw_below" :hash 2586408642)
 :void (draw-below bool))

(defgmethod
 (code-edit+get-text-for-code-completion :class 'code-edit :bind
  "get_text_for_code_completion" :hash 201670096)
 string)

(defgmethod
 (code-edit+request-code-completion :class 'code-edit :bind
  "request_code_completion" :hash 107499316)
 :void (force bool))

(defgmethod
 (code-edit+add-code-completion-option :class 'code-edit :bind
  "add_code_completion_option" :hash 3944379502)
 :void (type code-edit+code-completion-kind) (display-text string)
 (insert-text string) (text-color color) (icon resource) (value variant)
 (location int))

(defgmethod
 (code-edit+update-code-completion-options :class 'code-edit :bind
  "update_code_completion_options" :hash 2586408642)
 :void (force bool))

(defgmethod
 (code-edit+get-code-completion-options :class 'code-edit :bind
  "get_code_completion_options" :hash 3995934104)
 array)

(defgmethod
 (code-edit+get-code-completion-option :class 'code-edit :bind
  "get_code_completion_option" :hash 3485342025)
 dictionary (index int))

(defgmethod
 (code-edit+get-code-completion-selected-index :class 'code-edit :bind
  "get_code_completion_selected_index" :hash 3905245786)
 int)

(defgmethod
 (code-edit+set-code-completion-selected-index :class 'code-edit :bind
  "set_code_completion_selected_index" :hash 1286410249)
 :void (index int))

(defgmethod
 (code-edit+confirm-code-completion :class 'code-edit :bind
  "confirm_code_completion" :hash 107499316)
 :void (replace bool))

(defgmethod
 (code-edit+cancel-code-completion :class 'code-edit :bind
  "cancel_code_completion" :hash 3218959716)
 :void)

(defgmethod
 (code-edit+set-code-completion-enabled :class 'code-edit :bind
  "set_code_completion_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-code-completion-enabled :class 'code-edit :bind
  "is_code_completion_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+set-code-completion-prefixes :class 'code-edit :bind
  "set_code_completion_prefixes" :hash 381264803)
 :void (prefixes array))

(defgmethod
 (code-edit+get-code-completion-prefixes :class 'code-edit :bind
  "get_code_completion_prefixes" :hash 3995934104)
 array)

(defgmethod
 (code-edit+set-line-length-guidelines :class 'code-edit :bind
  "set_line_length_guidelines" :hash 381264803)
 :void (guideline-columns array))

(defgmethod
 (code-edit+get-line-length-guidelines :class 'code-edit :bind
  "get_line_length_guidelines" :hash 3995934104)
 array)

(defgmethod
 (code-edit+set-symbol-lookup-on-click-enabled :class 'code-edit :bind
  "set_symbol_lookup_on_click_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-symbol-lookup-on-click-enabled :class 'code-edit :bind
  "is_symbol_lookup_on_click_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+get-text-for-symbol-lookup :class 'code-edit :bind
  "get_text_for_symbol_lookup" :hash 201670096)
 string)

(defgmethod
 (code-edit+get-text-with-cursor-char :class 'code-edit :bind
  "get_text_with_cursor_char" :hash 1391810591)
 string (line int) (column int))

(defgmethod
 (code-edit+set-symbol-lookup-word-as-valid :class 'code-edit :bind
  "set_symbol_lookup_word_as_valid" :hash 2586408642)
 :void (valid bool))

(defgmethod
 (code-edit+set-symbol-tooltip-on-hover-enabled :class 'code-edit :bind
  "set_symbol_tooltip_on_hover_enabled" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (code-edit+is-symbol-tooltip-on-hover-enabled :class 'code-edit :bind
  "is_symbol_tooltip_on_hover_enabled" :hash 36873697)
 bool)

(defgmethod
 (code-edit+move-lines-up :class 'code-edit :bind "move_lines_up" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+move-lines-down :class 'code-edit :bind "move_lines_down" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+delete-lines :class 'code-edit :bind "delete_lines" :hash
  3218959716)
 :void)

(defgmethod
 (code-edit+duplicate-selection :class 'code-edit :bind "duplicate_selection"
  :hash 3218959716)
 :void)

(defgmethod
 (code-edit+duplicate-lines :class 'code-edit :bind "duplicate_lines" :hash
  3218959716)
 :void)