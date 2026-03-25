(common-lisp:in-package :%godot)


(defgmethod
 (tab-bar+set-tab-count :class 'tab-bar :bind "set_tab_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (tab-bar+get-tab-count :class 'tab-bar :bind "get_tab_count" :hash 3905245786)
 int)

(defgmethod
 (tab-bar+set-current-tab :class 'tab-bar :bind "set_current_tab" :hash
  1286410249)
 :void (tab-idx int))

(defgmethod
 (tab-bar+get-current-tab :class 'tab-bar :bind "get_current_tab" :hash
  3905245786)
 int)

(defgmethod
 (tab-bar+get-previous-tab :class 'tab-bar :bind "get_previous_tab" :hash
  3905245786)
 int)

(defgmethod
 (tab-bar+select-previous-available :class 'tab-bar :bind
  "select_previous_available" :hash 2240911060)
 bool)

(defgmethod
 (tab-bar+select-next-available :class 'tab-bar :bind "select_next_available"
  :hash 2240911060)
 bool)

(defgmethod
 (tab-bar+set-tab-title :class 'tab-bar :bind "set_tab_title" :hash 501894301)
 :void (tab-idx int) (title string))

(defgmethod
 (tab-bar+get-tab-title :class 'tab-bar :bind "get_tab_title" :hash 844755477)
 string (tab-idx int))

(defgmethod
 (tab-bar+set-tab-tooltip :class 'tab-bar :bind "set_tab_tooltip" :hash
  501894301)
 :void (tab-idx int) (tooltip string))

(defgmethod
 (tab-bar+get-tab-tooltip :class 'tab-bar :bind "get_tab_tooltip" :hash
  844755477)
 string (tab-idx int))

(defgmethod
 (tab-bar+set-tab-text-direction :class 'tab-bar :bind "set_tab_text_direction"
  :hash 1707680378)
 :void (tab-idx int) (direction control+text-direction))

(defgmethod
 (tab-bar+get-tab-text-direction :class 'tab-bar :bind "get_tab_text_direction"
  :hash 4235602388)
 control+text-direction (tab-idx int))

(defgmethod
 (tab-bar+set-tab-language :class 'tab-bar :bind "set_tab_language" :hash
  501894301)
 :void (tab-idx int) (language string))

(defgmethod
 (tab-bar+get-tab-language :class 'tab-bar :bind "get_tab_language" :hash
  844755477)
 string (tab-idx int))

(defgmethod
 (tab-bar+set-tab-icon :class 'tab-bar :bind "set_tab_icon" :hash 666127730)
 :void (tab-idx int) (icon texture-2d))

(defgmethod
 (tab-bar+get-tab-icon :class 'tab-bar :bind "get_tab_icon" :hash 3536238170)
 texture-2d (tab-idx int))

(defgmethod
 (tab-bar+set-tab-icon-max-width :class 'tab-bar :bind "set_tab_icon_max_width"
  :hash 3937882851)
 :void (tab-idx int) (width int))

(defgmethod
 (tab-bar+get-tab-icon-max-width :class 'tab-bar :bind "get_tab_icon_max_width"
  :hash 923996154)
 int (tab-idx int))

(defgmethod
 (tab-bar+set-tab-button-icon :class 'tab-bar :bind "set_tab_button_icon" :hash
  666127730)
 :void (tab-idx int) (icon texture-2d))

(defgmethod
 (tab-bar+get-tab-button-icon :class 'tab-bar :bind "get_tab_button_icon" :hash
  3536238170)
 texture-2d (tab-idx int))

(defgmethod
 (tab-bar+set-tab-disabled :class 'tab-bar :bind "set_tab_disabled" :hash
  300928843)
 :void (tab-idx int) (disabled bool))

(defgmethod
 (tab-bar+is-tab-disabled :class 'tab-bar :bind "is_tab_disabled" :hash
  1116898809)
 bool (tab-idx int))

(defgmethod
 (tab-bar+set-tab-hidden :class 'tab-bar :bind "set_tab_hidden" :hash
  300928843)
 :void (tab-idx int) (hidden bool))

(defgmethod
 (tab-bar+is-tab-hidden :class 'tab-bar :bind "is_tab_hidden" :hash 1116898809)
 bool (tab-idx int))

(defgmethod
 (tab-bar+set-tab-metadata :class 'tab-bar :bind "set_tab_metadata" :hash
  2152698145)
 :void (tab-idx int) (metadata variant))

(defgmethod
 (tab-bar+get-tab-metadata :class 'tab-bar :bind "get_tab_metadata" :hash
  4227898402)
 variant (tab-idx int))

(defgmethod
 (tab-bar+remove-tab :class 'tab-bar :bind "remove_tab" :hash 1286410249) :void
 (tab-idx int))

(defgmethod (tab-bar+add-tab :class 'tab-bar :bind "add_tab" :hash 1465444425)
 :void (title string) (icon texture-2d))

(defgmethod
 (tab-bar+get-tab-idx-at-point :class 'tab-bar :bind "get_tab_idx_at_point"
  :hash 3820158470)
 int (point vector-2))

(defgmethod
 (tab-bar+set-tab-alignment :class 'tab-bar :bind "set_tab_alignment" :hash
  2413632353)
 :void (alignment tab-bar+alignment-mode))

(defgmethod
 (tab-bar+get-tab-alignment :class 'tab-bar :bind "get_tab_alignment" :hash
  2178122193)
 tab-bar+alignment-mode)

(defgmethod
 (tab-bar+set-clip-tabs :class 'tab-bar :bind "set_clip_tabs" :hash 2586408642)
 :void (clip-tabs bool))

(defgmethod
 (tab-bar+get-clip-tabs :class 'tab-bar :bind "get_clip_tabs" :hash 36873697)
 bool)

(defgmethod
 (tab-bar+get-tab-offset :class 'tab-bar :bind "get_tab_offset" :hash
  3905245786)
 int)

(defgmethod
 (tab-bar+get-offset-buttons-visible :class 'tab-bar :bind
  "get_offset_buttons_visible" :hash 36873697)
 bool)

(defgmethod
 (tab-bar+ensure-tab-visible :class 'tab-bar :bind "ensure_tab_visible" :hash
  1286410249)
 :void (idx int))

(defgmethod
 (tab-bar+get-tab-rect :class 'tab-bar :bind "get_tab_rect" :hash 3327874267)
 rect-2 (tab-idx int))

(defgmethod
 (tab-bar+move-tab :class 'tab-bar :bind "move_tab" :hash 3937882851) :void
 (from int) (to int))

(defgmethod
 (tab-bar+set-close-with-middle-mouse :class 'tab-bar :bind
  "set_close_with_middle_mouse" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-close-with-middle-mouse :class 'tab-bar :bind
  "get_close_with_middle_mouse" :hash 36873697)
 bool)

(defgmethod
 (tab-bar+set-tab-close-display-policy :class 'tab-bar :bind
  "set_tab_close_display_policy" :hash 2212906737)
 :void (policy tab-bar+close-button-display-policy))

(defgmethod
 (tab-bar+get-tab-close-display-policy :class 'tab-bar :bind
  "get_tab_close_display_policy" :hash 2956568028)
 tab-bar+close-button-display-policy)

(defgmethod
 (tab-bar+set-max-tab-width :class 'tab-bar :bind "set_max_tab_width" :hash
  1286410249)
 :void (width int))

(defgmethod
 (tab-bar+get-max-tab-width :class 'tab-bar :bind "get_max_tab_width" :hash
  3905245786)
 int)

(defgmethod
 (tab-bar+set-scrolling-enabled :class 'tab-bar :bind "set_scrolling_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-scrolling-enabled :class 'tab-bar :bind "get_scrolling_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (tab-bar+set-drag-to-rearrange-enabled :class 'tab-bar :bind
  "set_drag_to_rearrange_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-drag-to-rearrange-enabled :class 'tab-bar :bind
  "get_drag_to_rearrange_enabled" :hash 36873697)
 bool)

(defgmethod
 (tab-bar+set-switch-on-drag-hover :class 'tab-bar :bind
  "set_switch_on_drag_hover" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-switch-on-drag-hover :class 'tab-bar :bind
  "get_switch_on_drag_hover" :hash 36873697)
 bool)

(defgmethod
 (tab-bar+set-tabs-rearrange-group :class 'tab-bar :bind
  "set_tabs_rearrange_group" :hash 1286410249)
 :void (group-id int))

(defgmethod
 (tab-bar+get-tabs-rearrange-group :class 'tab-bar :bind
  "get_tabs_rearrange_group" :hash 3905245786)
 int)

(defgmethod
 (tab-bar+set-scroll-to-selected :class 'tab-bar :bind "set_scroll_to_selected"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-scroll-to-selected :class 'tab-bar :bind "get_scroll_to_selected"
  :hash 36873697)
 bool)

(defgmethod
 (tab-bar+set-select-with-rmb :class 'tab-bar :bind "set_select_with_rmb" :hash
  2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-select-with-rmb :class 'tab-bar :bind "get_select_with_rmb" :hash
  36873697)
 bool)

(defgmethod
 (tab-bar+set-deselect-enabled :class 'tab-bar :bind "set_deselect_enabled"
  :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-bar+get-deselect-enabled :class 'tab-bar :bind "get_deselect_enabled"
  :hash 36873697)
 bool)

(defgmethod
 (tab-bar+clear-tabs :class 'tab-bar :bind "clear_tabs" :hash 3218959716) :void)