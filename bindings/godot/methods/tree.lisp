(common-lisp:in-package :%godot)


(defgmethod (tree+clear :class 'tree :bind "clear" :hash 3218959716) :void)

(defgmethod (tree+create-item :class 'tree :bind "create_item" :hash 528467046)
 tree-item (parent tree-item) (index int))

(defgmethod (tree+get-root :class 'tree :bind "get_root" :hash 1514277247)
 tree-item)

(defgmethod
 (tree+set-column-custom-minimum-width :class 'tree :bind
  "set_column_custom_minimum_width" :hash 3937882851)
 :void (column int) (min-width int))

(defgmethod
 (tree+set-column-expand :class 'tree :bind "set_column_expand" :hash
  300928843)
 :void (column int) (expand bool))

(defgmethod
 (tree+set-column-expand-ratio :class 'tree :bind "set_column_expand_ratio"
  :hash 3937882851)
 :void (column int) (ratio int))

(defgmethod
 (tree+set-column-clip-content :class 'tree :bind "set_column_clip_content"
  :hash 300928843)
 :void (column int) (enable bool))

(defgmethod
 (tree+is-column-expanding :class 'tree :bind "is_column_expanding" :hash
  1116898809)
 bool (column int))

(defgmethod
 (tree+is-column-clipping-content :class 'tree :bind
  "is_column_clipping_content" :hash 1116898809)
 bool (column int))

(defgmethod
 (tree+get-column-expand-ratio :class 'tree :bind "get_column_expand_ratio"
  :hash 923996154)
 int (column int))

(defgmethod
 (tree+get-column-width :class 'tree :bind "get_column_width" :hash 923996154)
 int (column int))

(defgmethod
 (tree+set-hide-root :class 'tree :bind "set_hide_root" :hash 2586408642) :void
 (enable bool))

(defgmethod
 (tree+is-root-hidden :class 'tree :bind "is_root_hidden" :hash 36873697) bool)

(defgmethod
 (tree+get-next-selected :class 'tree :bind "get_next_selected" :hash
  873446299)
 tree-item (from tree-item))

(defgmethod
 (tree+get-selected :class 'tree :bind "get_selected" :hash 1514277247)
 tree-item)

(defgmethod
 (tree+set-selected :class 'tree :bind "set_selected" :hash 2662547442) :void
 (item tree-item) (column int))

(defgmethod
 (tree+get-selected-column :class 'tree :bind "get_selected_column" :hash
  3905245786)
 int)

(defgmethod
 (tree+get-pressed-button :class 'tree :bind "get_pressed_button" :hash
  3905245786)
 int)

(defgmethod
 (tree+set-select-mode :class 'tree :bind "set_select_mode" :hash 3223887270)
 :void (mode tree+select-mode))

(defgmethod
 (tree+get-select-mode :class 'tree :bind "get_select_mode" :hash 100748571)
 tree+select-mode)

(defgmethod
 (tree+deselect-all :class 'tree :bind "deselect_all" :hash 3218959716) :void)

(defgmethod
 (tree+set-columns :class 'tree :bind "set_columns" :hash 1286410249) :void
 (amount int))

(defgmethod
 (tree+get-columns :class 'tree :bind "get_columns" :hash 3905245786) int)

(defgmethod (tree+get-edited :class 'tree :bind "get_edited" :hash 1514277247)
 tree-item)

(defgmethod
 (tree+get-edited-column :class 'tree :bind "get_edited_column" :hash
  3905245786)
 int)

(defgmethod
 (tree+edit-selected :class 'tree :bind "edit_selected" :hash 2595650253) bool
 (force-edit bool))

(defgmethod
 (tree+get-custom-popup-rect :class 'tree :bind "get_custom_popup_rect" :hash
  1639390495)
 rect-2)

(defgmethod
 (tree+get-item-area-rect :class 'tree :bind "get_item_area_rect" :hash
  47968679)
 rect-2 (item tree-item) (column int) (button-index int))

(defgmethod
 (tree+get-item-at-position :class 'tree :bind "get_item_at_position" :hash
  4193340126)
 tree-item (position vector-2))

(defgmethod
 (tree+get-column-at-position :class 'tree :bind "get_column_at_position" :hash
  3820158470)
 int (position vector-2))

(defgmethod
 (tree+get-drop-section-at-position :class 'tree :bind
  "get_drop_section_at_position" :hash 3820158470)
 int (position vector-2))

(defgmethod
 (tree+get-button-id-at-position :class 'tree :bind "get_button_id_at_position"
  :hash 3820158470)
 int (position vector-2))

(defgmethod
 (tree+ensure-cursor-is-visible :class 'tree :bind "ensure_cursor_is_visible"
  :hash 3218959716)
 :void)

(defgmethod
 (tree+set-column-titles-visible :class 'tree :bind "set_column_titles_visible"
  :hash 2586408642)
 :void (visible bool))

(defgmethod
 (tree+are-column-titles-visible :class 'tree :bind "are_column_titles_visible"
  :hash 36873697)
 bool)

(defgmethod
 (tree+set-column-title :class 'tree :bind "set_column_title" :hash 501894301)
 :void (column int) (title string))

(defgmethod
 (tree+get-column-title :class 'tree :bind "get_column_title" :hash 844755477)
 string (column int))

(defgmethod
 (tree+set-column-title-tooltip-text :class 'tree :bind
  "set_column_title_tooltip_text" :hash 501894301)
 :void (column int) (tooltip-text string))

(defgmethod
 (tree+get-column-title-tooltip-text :class 'tree :bind
  "get_column_title_tooltip_text" :hash 844755477)
 string (column int))

(defgmethod
 (tree+set-column-title-alignment :class 'tree :bind
  "set_column_title_alignment" :hash 3276431499)
 :void (column int) (title-alignment horizontal-alignment))

(defgmethod
 (tree+get-column-title-alignment :class 'tree :bind
  "get_column_title_alignment" :hash 4171562184)
 horizontal-alignment (column int))

(defgmethod
 (tree+set-column-title-direction :class 'tree :bind
  "set_column_title_direction" :hash 1707680378)
 :void (column int) (direction control+text-direction))

(defgmethod
 (tree+get-column-title-direction :class 'tree :bind
  "get_column_title_direction" :hash 4235602388)
 control+text-direction (column int))

(defgmethod
 (tree+set-column-title-language :class 'tree :bind "set_column_title_language"
  :hash 501894301)
 :void (column int) (language string))

(defgmethod
 (tree+get-column-title-language :class 'tree :bind "get_column_title_language"
  :hash 844755477)
 string (column int))

(defgmethod (tree+get-scroll :class 'tree :bind "get_scroll" :hash 3341600327)
 vector-2)

(defgmethod
 (tree+scroll-to-item :class 'tree :bind "scroll_to_item" :hash 1314737213)
 :void (item tree-item) (center-on-item bool))

(defgmethod
 (tree+set-h-scroll-enabled :class 'tree :bind "set_h_scroll_enabled" :hash
  2586408642)
 :void (h-scroll bool))

(defgmethod
 (tree+is-h-scroll-enabled :class 'tree :bind "is_h_scroll_enabled" :hash
  36873697)
 bool)

(defgmethod
 (tree+set-v-scroll-enabled :class 'tree :bind "set_v_scroll_enabled" :hash
  2586408642)
 :void (h-scroll bool))

(defgmethod
 (tree+is-v-scroll-enabled :class 'tree :bind "is_v_scroll_enabled" :hash
  36873697)
 bool)

(defgmethod
 (tree+set-scroll-hint-mode :class 'tree :bind "set_scroll_hint_mode" :hash
  415911924)
 :void (scroll-hint-mode tree+scroll-hint-mode))

(defgmethod
 (tree+get-scroll-hint-mode :class 'tree :bind "get_scroll_hint_mode" :hash
  553087187)
 tree+scroll-hint-mode)

(defgmethod
 (tree+set-tile-scroll-hint :class 'tree :bind "set_tile_scroll_hint" :hash
  2586408642)
 :void (tile-scroll-hint bool))

(defgmethod
 (tree+is-scroll-hint-tiled :class 'tree :bind "is_scroll_hint_tiled" :hash
  2240911060)
 bool)

(defgmethod
 (tree+set-hide-folding :class 'tree :bind "set_hide_folding" :hash 2586408642)
 :void (hide bool))

(defgmethod
 (tree+is-folding-hidden :class 'tree :bind "is_folding_hidden" :hash 36873697)
 bool)

(defgmethod
 (tree+set-enable-recursive-folding :class 'tree :bind
  "set_enable_recursive_folding" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (tree+is-recursive-folding-enabled :class 'tree :bind
  "is_recursive_folding_enabled" :hash 36873697)
 bool)

(defgmethod
 (tree+set-enable-drag-unfolding :class 'tree :bind "set_enable_drag_unfolding"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (tree+is-drag-unfolding-enabled :class 'tree :bind "is_drag_unfolding_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (tree+set-drop-mode-flags :class 'tree :bind "set_drop_mode_flags" :hash
  1286410249)
 :void (flags int))

(defgmethod
 (tree+get-drop-mode-flags :class 'tree :bind "get_drop_mode_flags" :hash
  3905245786)
 int)

(defgmethod
 (tree+set-allow-rmb-select :class 'tree :bind "set_allow_rmb_select" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (tree+get-allow-rmb-select :class 'tree :bind "get_allow_rmb_select" :hash
  36873697)
 bool)

(defgmethod
 (tree+set-allow-reselect :class 'tree :bind "set_allow_reselect" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (tree+get-allow-reselect :class 'tree :bind "get_allow_reselect" :hash
  36873697)
 bool)

(defgmethod
 (tree+set-allow-search :class 'tree :bind "set_allow_search" :hash 2586408642)
 :void (allow bool))

(defgmethod
 (tree+get-allow-search :class 'tree :bind "get_allow_search" :hash 36873697)
 bool)

(defgmethod
 (tree+set-auto-tooltip :class 'tree :bind "set_auto_tooltip" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (tree+is-auto-tooltip-enabled :class 'tree :bind "is_auto_tooltip_enabled"
  :hash 36873697)
 bool)