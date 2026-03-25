(common-lisp:in-package :%godot)


(defgmethod
 (tree-item+set-cell-mode :class 'tree-item :bind "set_cell_mode" :hash
  289920701)
 :void (column int) (mode tree-item+tree-cell-mode))

(defgmethod
 (tree-item+get-cell-mode :class 'tree-item :bind "get_cell_mode" :hash
  3406114978)
 tree-item+tree-cell-mode (column int))

(defgmethod
 (tree-item+set-auto-translate-mode :class 'tree-item :bind
  "set_auto_translate_mode" :hash 287402019)
 :void (column int) (mode node+auto-translate-mode))

(defgmethod
 (tree-item+get-auto-translate-mode :class 'tree-item :bind
  "get_auto_translate_mode" :hash 906302372)
 node+auto-translate-mode (column int))

(defgmethod
 (tree-item+set-edit-multiline :class 'tree-item :bind "set_edit_multiline"
  :hash 300928843)
 :void (column int) (multiline bool))

(defgmethod
 (tree-item+is-edit-multiline :class 'tree-item :bind "is_edit_multiline" :hash
  1116898809)
 bool (column int))

(defgmethod
 (tree-item+set-checked :class 'tree-item :bind "set_checked" :hash 300928843)
 :void (column int) (checked bool))

(defgmethod
 (tree-item+set-indeterminate :class 'tree-item :bind "set_indeterminate" :hash
  300928843)
 :void (column int) (indeterminate bool))

(defgmethod
 (tree-item+is-checked :class 'tree-item :bind "is_checked" :hash 1116898809)
 bool (column int))

(defgmethod
 (tree-item+is-indeterminate :class 'tree-item :bind "is_indeterminate" :hash
  1116898809)
 bool (column int))

(defgmethod
 (tree-item+propagate-check :class 'tree-item :bind "propagate_check" :hash
  972357352)
 :void (column int) (emit-signal bool))

(defgmethod
 (tree-item+set-text :class 'tree-item :bind "set_text" :hash 501894301) :void
 (column int) (text string))

(defgmethod
 (tree-item+get-text :class 'tree-item :bind "get_text" :hash 844755477) string
 (column int))

(defgmethod
 (tree-item+set-description :class 'tree-item :bind "set_description" :hash
  501894301)
 :void (column int) (description string))

(defgmethod
 (tree-item+get-description :class 'tree-item :bind "get_description" :hash
  844755477)
 string (column int))

(defgmethod
 (tree-item+set-text-direction :class 'tree-item :bind "set_text_direction"
  :hash 1707680378)
 :void (column int) (direction control+text-direction))

(defgmethod
 (tree-item+get-text-direction :class 'tree-item :bind "get_text_direction"
  :hash 4235602388)
 control+text-direction (column int))

(defgmethod
 (tree-item+set-autowrap-mode :class 'tree-item :bind "set_autowrap_mode" :hash
  3633006561)
 :void (column int) (autowrap-mode text-server+autowrap-mode))

(defgmethod
 (tree-item+get-autowrap-mode :class 'tree-item :bind "get_autowrap_mode" :hash
  2902757236)
 text-server+autowrap-mode (column int))

(defgmethod
 (tree-item+set-text-overrun-behavior :class 'tree-item :bind
  "set_text_overrun_behavior" :hash 1940772195)
 :void (column int) (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (tree-item+get-text-overrun-behavior :class 'tree-item :bind
  "get_text_overrun_behavior" :hash 3782727860)
 text-server+overrun-behavior (column int))

(defgmethod
 (tree-item+set-structured-text-bidi-override :class 'tree-item :bind
  "set_structured_text_bidi_override" :hash 868756907)
 :void (column int) (parser text-server+structured-text-parser))

(defgmethod
 (tree-item+get-structured-text-bidi-override :class 'tree-item :bind
  "get_structured_text_bidi_override" :hash 3377823772)
 text-server+structured-text-parser (column int))

(defgmethod
 (tree-item+set-structured-text-bidi-override-options :class 'tree-item :bind
  "set_structured_text_bidi_override_options" :hash 537221740)
 :void (column int) (args array))

(defgmethod
 (tree-item+get-structured-text-bidi-override-options :class 'tree-item :bind
  "get_structured_text_bidi_override_options" :hash 663333327)
 array (column int))

(defgmethod
 (tree-item+set-language :class 'tree-item :bind "set_language" :hash
  501894301)
 :void (column int) (language string))

(defgmethod
 (tree-item+get-language :class 'tree-item :bind "get_language" :hash
  844755477)
 string (column int))

(defgmethod
 (tree-item+set-suffix :class 'tree-item :bind "set_suffix" :hash 501894301)
 :void (column int) (text string))

(defgmethod
 (tree-item+get-suffix :class 'tree-item :bind "get_suffix" :hash 844755477)
 string (column int))

(defgmethod
 (tree-item+set-icon :class 'tree-item :bind "set_icon" :hash 666127730) :void
 (column int) (texture texture-2d))

(defgmethod
 (tree-item+get-icon :class 'tree-item :bind "get_icon" :hash 3536238170)
 texture-2d (column int))

(defgmethod
 (tree-item+set-icon-overlay :class 'tree-item :bind "set_icon_overlay" :hash
  666127730)
 :void (column int) (texture texture-2d))

(defgmethod
 (tree-item+get-icon-overlay :class 'tree-item :bind "get_icon_overlay" :hash
  3536238170)
 texture-2d (column int))

(defgmethod
 (tree-item+set-icon-region :class 'tree-item :bind "set_icon_region" :hash
  1356297692)
 :void (column int) (region rect-2))

(defgmethod
 (tree-item+get-icon-region :class 'tree-item :bind "get_icon_region" :hash
  3327874267)
 rect-2 (column int))

(defgmethod
 (tree-item+set-icon-max-width :class 'tree-item :bind "set_icon_max_width"
  :hash 3937882851)
 :void (column int) (width int))

(defgmethod
 (tree-item+get-icon-max-width :class 'tree-item :bind "get_icon_max_width"
  :hash 923996154)
 int (column int))

(defgmethod
 (tree-item+set-icon-modulate :class 'tree-item :bind "set_icon_modulate" :hash
  2878471219)
 :void (column int) (modulate color))

(defgmethod
 (tree-item+get-icon-modulate :class 'tree-item :bind "get_icon_modulate" :hash
  3457211756)
 color (column int))

(defgmethod
 (tree-item+set-range :class 'tree-item :bind "set_range" :hash 1602489585)
 :void (column int) (value float))

(defgmethod
 (tree-item+get-range :class 'tree-item :bind "get_range" :hash 2339986948)
 float (column int))

(defgmethod
 (tree-item+set-range-config :class 'tree-item :bind "set_range_config" :hash
  1547181014)
 :void (column int) (min float) (max float) (step float) (expr bool))

(defgmethod
 (tree-item+get-range-config :class 'tree-item :bind "get_range_config" :hash
  3554694381)
 dictionary (column int))

(defgmethod
 (tree-item+set-metadata :class 'tree-item :bind "set_metadata" :hash
  2152698145)
 :void (column int) (meta variant))

(defgmethod
 (tree-item+get-metadata :class 'tree-item :bind "get_metadata" :hash
  4227898402)
 variant (column int))

(defgmethod
 (tree-item+set-custom-draw :class 'tree-item :bind "set_custom_draw" :hash
  272420368)
 :void (column int) (object object) (callback string-name))

(defgmethod
 (tree-item+set-custom-draw-callback :class 'tree-item :bind
  "set_custom_draw_callback" :hash 957362965)
 :void (column int) (callback callable))

(defgmethod
 (tree-item+get-custom-draw-callback :class 'tree-item :bind
  "get_custom_draw_callback" :hash 1317077508)
 callable (column int))

(defgmethod
 (tree-item+set-custom-stylebox :class 'tree-item :bind "set_custom_stylebox"
  :hash 1433009359)
 :void (column int) (stylebox style-box))

(defgmethod
 (tree-item+get-custom-stylebox :class 'tree-item :bind "get_custom_stylebox"
  :hash 3362509644)
 style-box (column int))

(defgmethod
 (tree-item+set-collapsed :class 'tree-item :bind "set_collapsed" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (tree-item+is-collapsed :class 'tree-item :bind "is_collapsed" :hash
  2240911060)
 bool)

(defgmethod
 (tree-item+set-collapsed-recursive :class 'tree-item :bind
  "set_collapsed_recursive" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (tree-item+is-any-collapsed :class 'tree-item :bind "is_any_collapsed" :hash
  2595650253)
 bool (only-visible bool))

(defgmethod
 (tree-item+set-visible :class 'tree-item :bind "set_visible" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (tree-item+is-visible :class 'tree-item :bind "is_visible" :hash 2240911060)
 bool)

(defgmethod
 (tree-item+is-visible-in-tree :class 'tree-item :bind "is_visible_in_tree"
  :hash 36873697)
 bool)

(defgmethod
 (tree-item+uncollapse-tree :class 'tree-item :bind "uncollapse_tree" :hash
  3218959716)
 :void)

(defgmethod
 (tree-item+set-custom-minimum-height :class 'tree-item :bind
  "set_custom_minimum_height" :hash 1286410249)
 :void (height int))

(defgmethod
 (tree-item+get-custom-minimum-height :class 'tree-item :bind
  "get_custom_minimum_height" :hash 3905245786)
 int)

(defgmethod
 (tree-item+set-selectable :class 'tree-item :bind "set_selectable" :hash
  300928843)
 :void (column int) (selectable bool))

(defgmethod
 (tree-item+is-selectable :class 'tree-item :bind "is_selectable" :hash
  1116898809)
 bool (column int))

(defgmethod
 (tree-item+is-selected :class 'tree-item :bind "is_selected" :hash 3067735520)
 bool (column int))

(defgmethod
 (tree-item+select :class 'tree-item :bind "select" :hash 1286410249) :void
 (column int))

(defgmethod
 (tree-item+deselect :class 'tree-item :bind "deselect" :hash 1286410249) :void
 (column int))

(defgmethod
 (tree-item+set-editable :class 'tree-item :bind "set_editable" :hash
  300928843)
 :void (column int) (enabled bool))

(defgmethod
 (tree-item+is-editable :class 'tree-item :bind "is_editable" :hash 3067735520)
 bool (column int))

(defgmethod
 (tree-item+set-custom-color :class 'tree-item :bind "set_custom_color" :hash
  2878471219)
 :void (column int) (color color))

(defgmethod
 (tree-item+get-custom-color :class 'tree-item :bind "get_custom_color" :hash
  3457211756)
 color (column int))

(defgmethod
 (tree-item+clear-custom-color :class 'tree-item :bind "clear_custom_color"
  :hash 1286410249)
 :void (column int))

(defgmethod
 (tree-item+set-custom-font :class 'tree-item :bind "set_custom_font" :hash
  2637609184)
 :void (column int) (font font))

(defgmethod
 (tree-item+get-custom-font :class 'tree-item :bind "get_custom_font" :hash
  4244553094)
 font (column int))

(defgmethod
 (tree-item+set-custom-font-size :class 'tree-item :bind "set_custom_font_size"
  :hash 3937882851)
 :void (column int) (font-size int))

(defgmethod
 (tree-item+get-custom-font-size :class 'tree-item :bind "get_custom_font_size"
  :hash 923996154)
 int (column int))

(defgmethod
 (tree-item+set-custom-bg-color :class 'tree-item :bind "set_custom_bg_color"
  :hash 894174518)
 :void (column int) (color color) (just-outline bool))

(defgmethod
 (tree-item+clear-custom-bg-color :class 'tree-item :bind
  "clear_custom_bg_color" :hash 1286410249)
 :void (column int))

(defgmethod
 (tree-item+get-custom-bg-color :class 'tree-item :bind "get_custom_bg_color"
  :hash 3457211756)
 color (column int))

(defgmethod
 (tree-item+set-custom-as-button :class 'tree-item :bind "set_custom_as_button"
  :hash 300928843)
 :void (column int) (enable bool))

(defgmethod
 (tree-item+is-custom-set-as-button :class 'tree-item :bind
  "is_custom_set_as_button" :hash 1116898809)
 bool (column int))

(defgmethod
 (tree-item+clear-buttons :class 'tree-item :bind "clear_buttons" :hash
  3218959716)
 :void)

(defgmethod
 (tree-item+add-button :class 'tree-item :bind "add_button" :hash 973481897)
 :void (column int) (button texture-2d) (id int) (disabled bool)
 (tooltip-text string) (description string))

(defgmethod
 (tree-item+get-button-count :class 'tree-item :bind "get_button_count" :hash
  923996154)
 int (column int))

(defgmethod
 (tree-item+get-button-tooltip-text :class 'tree-item :bind
  "get_button_tooltip_text" :hash 1391810591)
 string (column int) (button-index int))

(defgmethod
 (tree-item+get-button-id :class 'tree-item :bind "get_button_id" :hash
  3175239445)
 int (column int) (button-index int))

(defgmethod
 (tree-item+get-button-by-id :class 'tree-item :bind "get_button_by_id" :hash
  3175239445)
 int (column int) (id int))

(defgmethod
 (tree-item+get-button-color :class 'tree-item :bind "get_button_color" :hash
  2165839948)
 color (column int) (id int))

(defgmethod
 (tree-item+get-button :class 'tree-item :bind "get_button" :hash 2584904275)
 texture-2d (column int) (button-index int))

(defgmethod
 (tree-item+set-button-tooltip-text :class 'tree-item :bind
  "set_button_tooltip_text" :hash 2285447957)
 :void (column int) (button-index int) (tooltip string))

(defgmethod
 (tree-item+set-button :class 'tree-item :bind "set_button" :hash 176101966)
 :void (column int) (button-index int) (button texture-2d))

(defgmethod
 (tree-item+erase-button :class 'tree-item :bind "erase_button" :hash
  3937882851)
 :void (column int) (button-index int))

(defgmethod
 (tree-item+set-button-description :class 'tree-item :bind
  "set_button_description" :hash 2285447957)
 :void (column int) (button-index int) (description string))

(defgmethod
 (tree-item+set-button-disabled :class 'tree-item :bind "set_button_disabled"
  :hash 1383440665)
 :void (column int) (button-index int) (disabled bool))

(defgmethod
 (tree-item+set-button-color :class 'tree-item :bind "set_button_color" :hash
  3733378741)
 :void (column int) (button-index int) (color color))

(defgmethod
 (tree-item+is-button-disabled :class 'tree-item :bind "is_button_disabled"
  :hash 2522259332)
 bool (column int) (button-index int))

(defgmethod
 (tree-item+set-tooltip-text :class 'tree-item :bind "set_tooltip_text" :hash
  501894301)
 :void (column int) (tooltip string))

(defgmethod
 (tree-item+get-tooltip-text :class 'tree-item :bind "get_tooltip_text" :hash
  844755477)
 string (column int))

(defgmethod
 (tree-item+set-text-alignment :class 'tree-item :bind "set_text_alignment"
  :hash 3276431499)
 :void (column int) (text-alignment horizontal-alignment))

(defgmethod
 (tree-item+get-text-alignment :class 'tree-item :bind "get_text_alignment"
  :hash 4171562184)
 horizontal-alignment (column int))

(defgmethod
 (tree-item+set-expand-right :class 'tree-item :bind "set_expand_right" :hash
  300928843)
 :void (column int) (enable bool))

(defgmethod
 (tree-item+get-expand-right :class 'tree-item :bind "get_expand_right" :hash
  1116898809)
 bool (column int))

(defgmethod
 (tree-item+set-disable-folding :class 'tree-item :bind "set_disable_folding"
  :hash 2586408642)
 :void (disable bool))

(defgmethod
 (tree-item+is-folding-disabled :class 'tree-item :bind "is_folding_disabled"
  :hash 36873697)
 bool)

(defgmethod
 (tree-item+create-child :class 'tree-item :bind "create_child" :hash
  954243986)
 tree-item (index int))

(defgmethod
 (tree-item+add-child :class 'tree-item :bind "add_child" :hash 1819951137)
 :void (child tree-item))

(defgmethod
 (tree-item+remove-child :class 'tree-item :bind "remove_child" :hash
  1819951137)
 :void (child tree-item))

(defgmethod
 (tree-item+get-tree :class 'tree-item :bind "get_tree" :hash 2243340556) tree)

(defgmethod
 (tree-item+get-next :class 'tree-item :bind "get_next" :hash 1514277247)
 tree-item)

(defgmethod
 (tree-item+get-prev :class 'tree-item :bind "get_prev" :hash 2768121250)
 tree-item)

(defgmethod
 (tree-item+get-parent :class 'tree-item :bind "get_parent" :hash 1514277247)
 tree-item)

(defgmethod
 (tree-item+get-first-child :class 'tree-item :bind "get_first_child" :hash
  1514277247)
 tree-item)

(defgmethod
 (tree-item+get-next-in-tree :class 'tree-item :bind "get_next_in_tree" :hash
  1666920593)
 tree-item (wrap bool))

(defgmethod
 (tree-item+get-prev-in-tree :class 'tree-item :bind "get_prev_in_tree" :hash
  1666920593)
 tree-item (wrap bool))

(defgmethod
 (tree-item+get-next-visible :class 'tree-item :bind "get_next_visible" :hash
  1666920593)
 tree-item (wrap bool))

(defgmethod
 (tree-item+get-prev-visible :class 'tree-item :bind "get_prev_visible" :hash
  1666920593)
 tree-item (wrap bool))

(defgmethod
 (tree-item+get-child :class 'tree-item :bind "get_child" :hash 306700752)
 tree-item (index int))

(defgmethod
 (tree-item+get-child-count :class 'tree-item :bind "get_child_count" :hash
  2455072627)
 int)

(defgmethod
 (tree-item+get-children :class 'tree-item :bind "get_children" :hash
  2915620761)
 array)

(defgmethod
 (tree-item+get-index :class 'tree-item :bind "get_index" :hash 2455072627) int)

(defgmethod
 (tree-item+move-before :class 'tree-item :bind "move_before" :hash 1819951137)
 :void (item tree-item))

(defgmethod
 (tree-item+move-after :class 'tree-item :bind "move_after" :hash 1819951137)
 :void (item tree-item))

(defgmethod
 (tree-item+call-recursive :class 'tree-item :bind "call_recursive" :hash
  2866548813 :vararg common-lisp:t)
 :void (method string-name))