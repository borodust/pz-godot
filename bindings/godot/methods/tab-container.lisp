(common-lisp:in-package :%godot)


(defgmethod
 (tab-container+get-tab-count :class 'tab-container :bind "get_tab_count" :hash
  3905245786)
 int)

(defgmethod
 (tab-container+set-current-tab :class 'tab-container :bind "set_current_tab"
  :hash 1286410249)
 :void (tab-idx int))

(defgmethod
 (tab-container+get-current-tab :class 'tab-container :bind "get_current_tab"
  :hash 3905245786)
 int)

(defgmethod
 (tab-container+get-previous-tab :class 'tab-container :bind "get_previous_tab"
  :hash 3905245786)
 int)

(defgmethod
 (tab-container+select-previous-available :class 'tab-container :bind
  "select_previous_available" :hash 2240911060)
 bool)

(defgmethod
 (tab-container+select-next-available :class 'tab-container :bind
  "select_next_available" :hash 2240911060)
 bool)

(defgmethod
 (tab-container+get-current-tab-control :class 'tab-container :bind
  "get_current_tab_control" :hash 2783021301)
 control)

(defgmethod
 (tab-container+get-tab-bar :class 'tab-container :bind "get_tab_bar" :hash
  1865451809)
 tab-bar)

(defgmethod
 (tab-container+get-tab-control :class 'tab-container :bind "get_tab_control"
  :hash 1065994134)
 control (tab-idx int))

(defgmethod
 (tab-container+set-tab-alignment :class 'tab-container :bind
  "set_tab_alignment" :hash 2413632353)
 :void (alignment tab-bar+alignment-mode))

(defgmethod
 (tab-container+get-tab-alignment :class 'tab-container :bind
  "get_tab_alignment" :hash 2178122193)
 tab-bar+alignment-mode)

(defgmethod
 (tab-container+set-tabs-position :class 'tab-container :bind
  "set_tabs_position" :hash 256673370)
 :void (tabs-position tab-container+tab-position))

(defgmethod
 (tab-container+get-tabs-position :class 'tab-container :bind
  "get_tabs_position" :hash 919937023)
 tab-container+tab-position)

(defgmethod
 (tab-container+set-clip-tabs :class 'tab-container :bind "set_clip_tabs" :hash
  2586408642)
 :void (clip-tabs bool))

(defgmethod
 (tab-container+get-clip-tabs :class 'tab-container :bind "get_clip_tabs" :hash
  36873697)
 bool)

(defgmethod
 (tab-container+set-tabs-visible :class 'tab-container :bind "set_tabs_visible"
  :hash 2586408642)
 :void (visible bool))

(defgmethod
 (tab-container+are-tabs-visible :class 'tab-container :bind "are_tabs_visible"
  :hash 36873697)
 bool)

(defgmethod
 (tab-container+set-all-tabs-in-front :class 'tab-container :bind
  "set_all_tabs_in_front" :hash 2586408642)
 :void (is-front bool))

(defgmethod
 (tab-container+is-all-tabs-in-front :class 'tab-container :bind
  "is_all_tabs_in_front" :hash 36873697)
 bool)

(defgmethod
 (tab-container+set-tab-title :class 'tab-container :bind "set_tab_title" :hash
  501894301)
 :void (tab-idx int) (title string))

(defgmethod
 (tab-container+get-tab-title :class 'tab-container :bind "get_tab_title" :hash
  844755477)
 string (tab-idx int))

(defgmethod
 (tab-container+set-tab-tooltip :class 'tab-container :bind "set_tab_tooltip"
  :hash 501894301)
 :void (tab-idx int) (tooltip string))

(defgmethod
 (tab-container+get-tab-tooltip :class 'tab-container :bind "get_tab_tooltip"
  :hash 844755477)
 string (tab-idx int))

(defgmethod
 (tab-container+set-tab-icon :class 'tab-container :bind "set_tab_icon" :hash
  666127730)
 :void (tab-idx int) (icon texture-2d))

(defgmethod
 (tab-container+get-tab-icon :class 'tab-container :bind "get_tab_icon" :hash
  3536238170)
 texture-2d (tab-idx int))

(defgmethod
 (tab-container+set-tab-icon-max-width :class 'tab-container :bind
  "set_tab_icon_max_width" :hash 3937882851)
 :void (tab-idx int) (width int))

(defgmethod
 (tab-container+get-tab-icon-max-width :class 'tab-container :bind
  "get_tab_icon_max_width" :hash 923996154)
 int (tab-idx int))

(defgmethod
 (tab-container+set-tab-disabled :class 'tab-container :bind "set_tab_disabled"
  :hash 300928843)
 :void (tab-idx int) (disabled bool))

(defgmethod
 (tab-container+is-tab-disabled :class 'tab-container :bind "is_tab_disabled"
  :hash 1116898809)
 bool (tab-idx int))

(defgmethod
 (tab-container+set-tab-hidden :class 'tab-container :bind "set_tab_hidden"
  :hash 300928843)
 :void (tab-idx int) (hidden bool))

(defgmethod
 (tab-container+is-tab-hidden :class 'tab-container :bind "is_tab_hidden" :hash
  1116898809)
 bool (tab-idx int))

(defgmethod
 (tab-container+set-tab-metadata :class 'tab-container :bind "set_tab_metadata"
  :hash 2152698145)
 :void (tab-idx int) (metadata variant))

(defgmethod
 (tab-container+get-tab-metadata :class 'tab-container :bind "get_tab_metadata"
  :hash 4227898402)
 variant (tab-idx int))

(defgmethod
 (tab-container+set-tab-button-icon :class 'tab-container :bind
  "set_tab_button_icon" :hash 666127730)
 :void (tab-idx int) (icon texture-2d))

(defgmethod
 (tab-container+get-tab-button-icon :class 'tab-container :bind
  "get_tab_button_icon" :hash 3536238170)
 texture-2d (tab-idx int))

(defgmethod
 (tab-container+get-tab-idx-at-point :class 'tab-container :bind
  "get_tab_idx_at_point" :hash 3820158470)
 int (point vector-2))

(defgmethod
 (tab-container+get-tab-idx-from-control :class 'tab-container :bind
  "get_tab_idx_from_control" :hash 2787397975)
 int (control control))

(defgmethod
 (tab-container+set-popup :class 'tab-container :bind "set_popup" :hash
  1078189570)
 :void (popup node))

(defgmethod
 (tab-container+get-popup :class 'tab-container :bind "get_popup" :hash
  111095082)
 popup)

(defgmethod
 (tab-container+set-switch-on-drag-hover :class 'tab-container :bind
  "set_switch_on_drag_hover" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-container+get-switch-on-drag-hover :class 'tab-container :bind
  "get_switch_on_drag_hover" :hash 36873697)
 bool)

(defgmethod
 (tab-container+set-drag-to-rearrange-enabled :class 'tab-container :bind
  "set_drag_to_rearrange_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-container+get-drag-to-rearrange-enabled :class 'tab-container :bind
  "get_drag_to_rearrange_enabled" :hash 36873697)
 bool)

(defgmethod
 (tab-container+set-tabs-rearrange-group :class 'tab-container :bind
  "set_tabs_rearrange_group" :hash 1286410249)
 :void (group-id int))

(defgmethod
 (tab-container+get-tabs-rearrange-group :class 'tab-container :bind
  "get_tabs_rearrange_group" :hash 3905245786)
 int)

(defgmethod
 (tab-container+set-use-hidden-tabs-for-min-size :class 'tab-container :bind
  "set_use_hidden_tabs_for_min_size" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-container+get-use-hidden-tabs-for-min-size :class 'tab-container :bind
  "get_use_hidden_tabs_for_min_size" :hash 36873697)
 bool)

(defgmethod
 (tab-container+set-tab-focus-mode :class 'tab-container :bind
  "set_tab_focus_mode" :hash 3232914922)
 :void (focus-mode control+focus-mode))

(defgmethod
 (tab-container+get-tab-focus-mode :class 'tab-container :bind
  "get_tab_focus_mode" :hash 2132829277)
 control+focus-mode)

(defgmethod
 (tab-container+set-deselect-enabled :class 'tab-container :bind
  "set_deselect_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (tab-container+get-deselect-enabled :class 'tab-container :bind
  "get_deselect_enabled" :hash 36873697)
 bool)