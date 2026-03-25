(common-lisp:in-package :%godot)


(defgmethod
 (item-list+add-item :class 'item-list :bind "add_item" :hash 359861678) int
 (text string) (icon texture-2d) (selectable bool))

(defgmethod
 (item-list+add-icon-item :class 'item-list :bind "add_icon_item" :hash
  4256579627)
 int (icon texture-2d) (selectable bool))

(defgmethod
 (item-list+set-item-text :class 'item-list :bind "set_item_text" :hash
  501894301)
 :void (idx int) (text string))

(defgmethod
 (item-list+get-item-text :class 'item-list :bind "get_item_text" :hash
  844755477)
 string (idx int))

(defgmethod
 (item-list+set-item-icon :class 'item-list :bind "set_item_icon" :hash
  666127730)
 :void (idx int) (icon texture-2d))

(defgmethod
 (item-list+get-item-icon :class 'item-list :bind "get_item_icon" :hash
  3536238170)
 texture-2d (idx int))

(defgmethod
 (item-list+set-item-text-direction :class 'item-list :bind
  "set_item_text_direction" :hash 1707680378)
 :void (idx int) (direction control+text-direction))

(defgmethod
 (item-list+get-item-text-direction :class 'item-list :bind
  "get_item_text_direction" :hash 4235602388)
 control+text-direction (idx int))

(defgmethod
 (item-list+set-item-language :class 'item-list :bind "set_item_language" :hash
  501894301)
 :void (idx int) (language string))

(defgmethod
 (item-list+get-item-language :class 'item-list :bind "get_item_language" :hash
  844755477)
 string (idx int))

(defgmethod
 (item-list+set-item-auto-translate-mode :class 'item-list :bind
  "set_item_auto_translate_mode" :hash 287402019)
 :void (idx int) (mode node+auto-translate-mode))

(defgmethod
 (item-list+get-item-auto-translate-mode :class 'item-list :bind
  "get_item_auto_translate_mode" :hash 906302372)
 node+auto-translate-mode (idx int))

(defgmethod
 (item-list+set-item-icon-transposed :class 'item-list :bind
  "set_item_icon_transposed" :hash 300928843)
 :void (idx int) (transposed bool))

(defgmethod
 (item-list+is-item-icon-transposed :class 'item-list :bind
  "is_item_icon_transposed" :hash 1116898809)
 bool (idx int))

(defgmethod
 (item-list+set-item-icon-region :class 'item-list :bind "set_item_icon_region"
  :hash 1356297692)
 :void (idx int) (rect rect-2))

(defgmethod
 (item-list+get-item-icon-region :class 'item-list :bind "get_item_icon_region"
  :hash 3327874267)
 rect-2 (idx int))

(defgmethod
 (item-list+set-item-icon-modulate :class 'item-list :bind
  "set_item_icon_modulate" :hash 2878471219)
 :void (idx int) (modulate color))

(defgmethod
 (item-list+get-item-icon-modulate :class 'item-list :bind
  "get_item_icon_modulate" :hash 3457211756)
 color (idx int))

(defgmethod
 (item-list+set-item-selectable :class 'item-list :bind "set_item_selectable"
  :hash 300928843)
 :void (idx int) (selectable bool))

(defgmethod
 (item-list+is-item-selectable :class 'item-list :bind "is_item_selectable"
  :hash 1116898809)
 bool (idx int))

(defgmethod
 (item-list+set-item-disabled :class 'item-list :bind "set_item_disabled" :hash
  300928843)
 :void (idx int) (disabled bool))

(defgmethod
 (item-list+is-item-disabled :class 'item-list :bind "is_item_disabled" :hash
  1116898809)
 bool (idx int))

(defgmethod
 (item-list+set-item-metadata :class 'item-list :bind "set_item_metadata" :hash
  2152698145)
 :void (idx int) (metadata variant))

(defgmethod
 (item-list+get-item-metadata :class 'item-list :bind "get_item_metadata" :hash
  4227898402)
 variant (idx int))

(defgmethod
 (item-list+set-item-custom-bg-color :class 'item-list :bind
  "set_item_custom_bg_color" :hash 2878471219)
 :void (idx int) (custom-bg-color color))

(defgmethod
 (item-list+get-item-custom-bg-color :class 'item-list :bind
  "get_item_custom_bg_color" :hash 3457211756)
 color (idx int))

(defgmethod
 (item-list+set-item-custom-fg-color :class 'item-list :bind
  "set_item_custom_fg_color" :hash 2878471219)
 :void (idx int) (custom-fg-color color))

(defgmethod
 (item-list+get-item-custom-fg-color :class 'item-list :bind
  "get_item_custom_fg_color" :hash 3457211756)
 color (idx int))

(defgmethod
 (item-list+get-item-rect :class 'item-list :bind "get_item_rect" :hash
  159227807)
 rect-2 (idx int) (expand bool))

(defgmethod
 (item-list+set-item-tooltip-enabled :class 'item-list :bind
  "set_item_tooltip_enabled" :hash 300928843)
 :void (idx int) (enable bool))

(defgmethod
 (item-list+is-item-tooltip-enabled :class 'item-list :bind
  "is_item_tooltip_enabled" :hash 1116898809)
 bool (idx int))

(defgmethod
 (item-list+set-item-tooltip :class 'item-list :bind "set_item_tooltip" :hash
  501894301)
 :void (idx int) (tooltip string))

(defgmethod
 (item-list+get-item-tooltip :class 'item-list :bind "get_item_tooltip" :hash
  844755477)
 string (idx int))

(defgmethod (item-list+select :class 'item-list :bind "select" :hash 972357352)
 :void (idx int) (single bool))

(defgmethod
 (item-list+deselect :class 'item-list :bind "deselect" :hash 1286410249) :void
 (idx int))

(defgmethod
 (item-list+deselect-all :class 'item-list :bind "deselect_all" :hash
  3218959716)
 :void)

(defgmethod
 (item-list+is-selected :class 'item-list :bind "is_selected" :hash 1116898809)
 bool (idx int))

(defgmethod
 (item-list+get-selected-items :class 'item-list :bind "get_selected_items"
  :hash 969006518)
 packed-int-32array)

(defgmethod
 (item-list+move-item :class 'item-list :bind "move_item" :hash 3937882851)
 :void (from-idx int) (to-idx int))

(defgmethod
 (item-list+set-item-count :class 'item-list :bind "set_item_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (item-list+get-item-count :class 'item-list :bind "get_item_count" :hash
  3905245786)
 int)

(defgmethod
 (item-list+remove-item :class 'item-list :bind "remove_item" :hash 1286410249)
 :void (idx int))

(defgmethod (item-list+clear :class 'item-list :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (item-list+sort-items-by-text :class 'item-list :bind "sort_items_by_text"
  :hash 3218959716)
 :void)

(defgmethod
 (item-list+set-fixed-column-width :class 'item-list :bind
  "set_fixed_column_width" :hash 1286410249)
 :void (width int))

(defgmethod
 (item-list+get-fixed-column-width :class 'item-list :bind
  "get_fixed_column_width" :hash 3905245786)
 int)

(defgmethod
 (item-list+set-same-column-width :class 'item-list :bind
  "set_same_column_width" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (item-list+is-same-column-width :class 'item-list :bind "is_same_column_width"
  :hash 36873697)
 bool)

(defgmethod
 (item-list+set-max-text-lines :class 'item-list :bind "set_max_text_lines"
  :hash 1286410249)
 :void (lines int))

(defgmethod
 (item-list+get-max-text-lines :class 'item-list :bind "get_max_text_lines"
  :hash 3905245786)
 int)

(defgmethod
 (item-list+set-max-columns :class 'item-list :bind "set_max_columns" :hash
  1286410249)
 :void (amount int))

(defgmethod
 (item-list+get-max-columns :class 'item-list :bind "get_max_columns" :hash
  3905245786)
 int)

(defgmethod
 (item-list+set-select-mode :class 'item-list :bind "set_select_mode" :hash
  928267388)
 :void (mode item-list+select-mode))

(defgmethod
 (item-list+get-select-mode :class 'item-list :bind "get_select_mode" :hash
  1191945842)
 item-list+select-mode)

(defgmethod
 (item-list+set-icon-mode :class 'item-list :bind "set_icon_mode" :hash
  2025053633)
 :void (mode item-list+icon-mode))

(defgmethod
 (item-list+get-icon-mode :class 'item-list :bind "get_icon_mode" :hash
  3353929232)
 item-list+icon-mode)

(defgmethod
 (item-list+set-fixed-icon-size :class 'item-list :bind "set_fixed_icon_size"
  :hash 1130785943)
 :void (size vector-2i))

(defgmethod
 (item-list+get-fixed-icon-size :class 'item-list :bind "get_fixed_icon_size"
  :hash 3690982128)
 vector-2i)

(defgmethod
 (item-list+set-icon-scale :class 'item-list :bind "set_icon_scale" :hash
  373806689)
 :void (scale float))

(defgmethod
 (item-list+get-icon-scale :class 'item-list :bind "get_icon_scale" :hash
  1740695150)
 float)

(defgmethod
 (item-list+set-allow-rmb-select :class 'item-list :bind "set_allow_rmb_select"
  :hash 2586408642)
 :void (allow bool))

(defgmethod
 (item-list+get-allow-rmb-select :class 'item-list :bind "get_allow_rmb_select"
  :hash 36873697)
 bool)

(defgmethod
 (item-list+set-allow-reselect :class 'item-list :bind "set_allow_reselect"
  :hash 2586408642)
 :void (allow bool))

(defgmethod
 (item-list+get-allow-reselect :class 'item-list :bind "get_allow_reselect"
  :hash 36873697)
 bool)

(defgmethod
 (item-list+set-allow-search :class 'item-list :bind "set_allow_search" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (item-list+get-allow-search :class 'item-list :bind "get_allow_search" :hash
  36873697)
 bool)

(defgmethod
 (item-list+set-auto-width :class 'item-list :bind "set_auto_width" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (item-list+has-auto-width :class 'item-list :bind "has_auto_width" :hash
  36873697)
 bool)

(defgmethod
 (item-list+set-auto-height :class 'item-list :bind "set_auto_height" :hash
  2586408642)
 :void (enable bool))

(defgmethod
 (item-list+has-auto-height :class 'item-list :bind "has_auto_height" :hash
  36873697)
 bool)

(defgmethod
 (item-list+is-anything-selected :class 'item-list :bind "is_anything_selected"
  :hash 2240911060)
 bool)

(defgmethod
 (item-list+get-item-at-position :class 'item-list :bind "get_item_at_position"
  :hash 2300324924)
 int (position vector-2) (exact bool))

(defgmethod
 (item-list+ensure-current-is-visible :class 'item-list :bind
  "ensure_current_is_visible" :hash 3218959716)
 :void)

(defgmethod
 (item-list+get-v-scroll-bar :class 'item-list :bind "get_v_scroll_bar" :hash
  2630340773)
 vscroll-bar)

(defgmethod
 (item-list+get-h-scroll-bar :class 'item-list :bind "get_h_scroll_bar" :hash
  4004517983)
 hscroll-bar)

(defgmethod
 (item-list+set-scroll-hint-mode :class 'item-list :bind "set_scroll_hint_mode"
  :hash 2917787337)
 :void (scroll-hint-mode item-list+scroll-hint-mode))

(defgmethod
 (item-list+get-scroll-hint-mode :class 'item-list :bind "get_scroll_hint_mode"
  :hash 2522227939)
 item-list+scroll-hint-mode)

(defgmethod
 (item-list+set-tile-scroll-hint :class 'item-list :bind "set_tile_scroll_hint"
  :hash 2586408642)
 :void (tile-scroll-hint bool))

(defgmethod
 (item-list+is-scroll-hint-tiled :class 'item-list :bind "is_scroll_hint_tiled"
  :hash 2240911060)
 bool)

(defgmethod
 (item-list+set-text-overrun-behavior :class 'item-list :bind
  "set_text_overrun_behavior" :hash 1008890932)
 :void (overrun-behavior text-server+overrun-behavior))

(defgmethod
 (item-list+get-text-overrun-behavior :class 'item-list :bind
  "get_text_overrun_behavior" :hash 3779142101)
 text-server+overrun-behavior)

(defgmethod
 (item-list+set-wraparound-items :class 'item-list :bind "set_wraparound_items"
  :hash 2586408642)
 :void (enable bool))

(defgmethod
 (item-list+has-wraparound-items :class 'item-list :bind "has_wraparound_items"
  :hash 36873697)
 bool)

(defgmethod
 (item-list+force-update-list-size :class 'item-list :bind
  "force_update_list_size" :hash 3218959716)
 :void)