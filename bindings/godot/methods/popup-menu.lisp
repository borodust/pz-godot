(common-lisp:in-package :%godot)


(defgmethod
 (popup-menu+activate-item-by-event :class 'popup-menu :bind
  "activate_item_by_event" :hash 3716412023)
 bool (event input-event) (for-global-only bool))

(defgmethod
 (popup-menu+set-prefer-native-menu :class 'popup-menu :bind
  "set_prefer_native_menu" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (popup-menu+is-prefer-native-menu :class 'popup-menu :bind
  "is_prefer_native_menu" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+is-native-menu :class 'popup-menu :bind "is_native_menu" :hash
  36873697)
 bool)

(defgmethod
 (popup-menu+add-item :class 'popup-menu :bind "add_item" :hash 3674230041)
 :void (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-icon-item :class 'popup-menu :bind "add_icon_item" :hash
  1086190128)
 :void (texture texture-2d) (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-check-item :class 'popup-menu :bind "add_check_item" :hash
  3674230041)
 :void (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-icon-check-item :class 'popup-menu :bind "add_icon_check_item"
  :hash 1086190128)
 :void (texture texture-2d) (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-radio-check-item :class 'popup-menu :bind
  "add_radio_check_item" :hash 3674230041)
 :void (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-icon-radio-check-item :class 'popup-menu :bind
  "add_icon_radio_check_item" :hash 1086190128)
 :void (texture texture-2d) (label string) (id int) (accel key))

(defgmethod
 (popup-menu+add-multistate-item :class 'popup-menu :bind "add_multistate_item"
  :hash 150780458)
 :void (label string) (max-states int) (default-state int) (id int) (accel key))

(defgmethod
 (popup-menu+add-shortcut :class 'popup-menu :bind "add_shortcut" :hash
  3451850107)
 :void (shortcut shortcut) (id int) (global bool) (allow-echo bool))

(defgmethod
 (popup-menu+add-icon-shortcut :class 'popup-menu :bind "add_icon_shortcut"
  :hash 2997871092)
 :void (texture texture-2d) (shortcut shortcut) (id int) (global bool)
 (allow-echo bool))

(defgmethod
 (popup-menu+add-check-shortcut :class 'popup-menu :bind "add_check_shortcut"
  :hash 1642193386)
 :void (shortcut shortcut) (id int) (global bool))

(defgmethod
 (popup-menu+add-icon-check-shortcut :class 'popup-menu :bind
  "add_icon_check_shortcut" :hash 3856247530)
 :void (texture texture-2d) (shortcut shortcut) (id int) (global bool))

(defgmethod
 (popup-menu+add-radio-check-shortcut :class 'popup-menu :bind
  "add_radio_check_shortcut" :hash 1642193386)
 :void (shortcut shortcut) (id int) (global bool))

(defgmethod
 (popup-menu+add-icon-radio-check-shortcut :class 'popup-menu :bind
  "add_icon_radio_check_shortcut" :hash 3856247530)
 :void (texture texture-2d) (shortcut shortcut) (id int) (global bool))

(defgmethod
 (popup-menu+add-submenu-item :class 'popup-menu :bind "add_submenu_item" :hash
  2979222410)
 :void (label string) (submenu string) (id int))

(defgmethod
 (popup-menu+add-submenu-node-item :class 'popup-menu :bind
  "add_submenu_node_item" :hash 1325455216)
 :void (label string) (submenu popup-menu) (id int))

(defgmethod
 (popup-menu+set-item-text :class 'popup-menu :bind "set_item_text" :hash
  501894301)
 :void (index int) (text string))

(defgmethod
 (popup-menu+set-item-text-direction :class 'popup-menu :bind
  "set_item_text_direction" :hash 1707680378)
 :void (index int) (direction control+text-direction))

(defgmethod
 (popup-menu+set-item-language :class 'popup-menu :bind "set_item_language"
  :hash 501894301)
 :void (index int) (language string))

(defgmethod
 (popup-menu+set-item-auto-translate-mode :class 'popup-menu :bind
  "set_item_auto_translate_mode" :hash 287402019)
 :void (index int) (mode node+auto-translate-mode))

(defgmethod
 (popup-menu+set-item-icon :class 'popup-menu :bind "set_item_icon" :hash
  666127730)
 :void (index int) (icon texture-2d))

(defgmethod
 (popup-menu+set-item-icon-max-width :class 'popup-menu :bind
  "set_item_icon_max_width" :hash 3937882851)
 :void (index int) (width int))

(defgmethod
 (popup-menu+set-item-icon-modulate :class 'popup-menu :bind
  "set_item_icon_modulate" :hash 2878471219)
 :void (index int) (modulate color))

(defgmethod
 (popup-menu+set-item-checked :class 'popup-menu :bind "set_item_checked" :hash
  300928843)
 :void (index int) (checked bool))

(defgmethod
 (popup-menu+set-item-id :class 'popup-menu :bind "set_item_id" :hash
  3937882851)
 :void (index int) (id int))

(defgmethod
 (popup-menu+set-item-accelerator :class 'popup-menu :bind
  "set_item_accelerator" :hash 2992817551)
 :void (index int) (accel key))

(defgmethod
 (popup-menu+set-item-metadata :class 'popup-menu :bind "set_item_metadata"
  :hash 2152698145)
 :void (index int) (metadata variant))

(defgmethod
 (popup-menu+set-item-disabled :class 'popup-menu :bind "set_item_disabled"
  :hash 300928843)
 :void (index int) (disabled bool))

(defgmethod
 (popup-menu+set-item-submenu :class 'popup-menu :bind "set_item_submenu" :hash
  501894301)
 :void (index int) (submenu string))

(defgmethod
 (popup-menu+set-item-submenu-node :class 'popup-menu :bind
  "set_item_submenu_node" :hash 1068370740)
 :void (index int) (submenu popup-menu))

(defgmethod
 (popup-menu+set-item-as-separator :class 'popup-menu :bind
  "set_item_as_separator" :hash 300928843)
 :void (index int) (enable bool))

(defgmethod
 (popup-menu+set-item-as-checkable :class 'popup-menu :bind
  "set_item_as_checkable" :hash 300928843)
 :void (index int) (enable bool))

(defgmethod
 (popup-menu+set-item-as-radio-checkable :class 'popup-menu :bind
  "set_item_as_radio_checkable" :hash 300928843)
 :void (index int) (enable bool))

(defgmethod
 (popup-menu+set-item-tooltip :class 'popup-menu :bind "set_item_tooltip" :hash
  501894301)
 :void (index int) (tooltip string))

(defgmethod
 (popup-menu+set-item-shortcut :class 'popup-menu :bind "set_item_shortcut"
  :hash 825127832)
 :void (index int) (shortcut shortcut) (global bool))

(defgmethod
 (popup-menu+set-item-indent :class 'popup-menu :bind "set_item_indent" :hash
  3937882851)
 :void (index int) (indent int))

(defgmethod
 (popup-menu+set-item-multistate :class 'popup-menu :bind "set_item_multistate"
  :hash 3937882851)
 :void (index int) (state int))

(defgmethod
 (popup-menu+set-item-multistate-max :class 'popup-menu :bind
  "set_item_multistate_max" :hash 3937882851)
 :void (index int) (max-states int))

(defgmethod
 (popup-menu+set-item-shortcut-disabled :class 'popup-menu :bind
  "set_item_shortcut_disabled" :hash 300928843)
 :void (index int) (disabled bool))

(defgmethod
 (popup-menu+set-item-index :class 'popup-menu :bind "set_item_index" :hash
  3937882851)
 :void (index int) (target-index int))

(defgmethod
 (popup-menu+toggle-item-checked :class 'popup-menu :bind "toggle_item_checked"
  :hash 1286410249)
 :void (index int))

(defgmethod
 (popup-menu+toggle-item-multistate :class 'popup-menu :bind
  "toggle_item_multistate" :hash 1286410249)
 :void (index int))

(defgmethod
 (popup-menu+get-item-text :class 'popup-menu :bind "get_item_text" :hash
  844755477)
 string (index int))

(defgmethod
 (popup-menu+get-item-text-direction :class 'popup-menu :bind
  "get_item_text_direction" :hash 4235602388)
 control+text-direction (index int))

(defgmethod
 (popup-menu+get-item-language :class 'popup-menu :bind "get_item_language"
  :hash 844755477)
 string (index int))

(defgmethod
 (popup-menu+get-item-auto-translate-mode :class 'popup-menu :bind
  "get_item_auto_translate_mode" :hash 906302372)
 node+auto-translate-mode (index int))

(defgmethod
 (popup-menu+get-item-icon :class 'popup-menu :bind "get_item_icon" :hash
  3536238170)
 texture-2d (index int))

(defgmethod
 (popup-menu+get-item-icon-max-width :class 'popup-menu :bind
  "get_item_icon_max_width" :hash 923996154)
 int (index int))

(defgmethod
 (popup-menu+get-item-icon-modulate :class 'popup-menu :bind
  "get_item_icon_modulate" :hash 3457211756)
 color (index int))

(defgmethod
 (popup-menu+is-item-checked :class 'popup-menu :bind "is_item_checked" :hash
  1116898809)
 bool (index int))

(defgmethod
 (popup-menu+get-item-id :class 'popup-menu :bind "get_item_id" :hash
  923996154)
 int (index int))

(defgmethod
 (popup-menu+get-item-index :class 'popup-menu :bind "get_item_index" :hash
  923996154)
 int (id int))

(defgmethod
 (popup-menu+get-item-accelerator :class 'popup-menu :bind
  "get_item_accelerator" :hash 253789942)
 key (index int))

(defgmethod
 (popup-menu+get-item-metadata :class 'popup-menu :bind "get_item_metadata"
  :hash 4227898402)
 variant (index int))

(defgmethod
 (popup-menu+is-item-disabled :class 'popup-menu :bind "is_item_disabled" :hash
  1116898809)
 bool (index int))

(defgmethod
 (popup-menu+get-item-submenu :class 'popup-menu :bind "get_item_submenu" :hash
  844755477)
 string (index int))

(defgmethod
 (popup-menu+get-item-submenu-node :class 'popup-menu :bind
  "get_item_submenu_node" :hash 2100501353)
 popup-menu (index int))

(defgmethod
 (popup-menu+is-item-separator :class 'popup-menu :bind "is_item_separator"
  :hash 1116898809)
 bool (index int))

(defgmethod
 (popup-menu+is-item-checkable :class 'popup-menu :bind "is_item_checkable"
  :hash 1116898809)
 bool (index int))

(defgmethod
 (popup-menu+is-item-radio-checkable :class 'popup-menu :bind
  "is_item_radio_checkable" :hash 1116898809)
 bool (index int))

(defgmethod
 (popup-menu+is-item-shortcut-disabled :class 'popup-menu :bind
  "is_item_shortcut_disabled" :hash 1116898809)
 bool (index int))

(defgmethod
 (popup-menu+get-item-tooltip :class 'popup-menu :bind "get_item_tooltip" :hash
  844755477)
 string (index int))

(defgmethod
 (popup-menu+get-item-shortcut :class 'popup-menu :bind "get_item_shortcut"
  :hash 1449483325)
 shortcut (index int))

(defgmethod
 (popup-menu+get-item-indent :class 'popup-menu :bind "get_item_indent" :hash
  923996154)
 int (index int))

(defgmethod
 (popup-menu+get-item-multistate-max :class 'popup-menu :bind
  "get_item_multistate_max" :hash 923996154)
 int (index int))

(defgmethod
 (popup-menu+get-item-multistate :class 'popup-menu :bind "get_item_multistate"
  :hash 923996154)
 int (index int))

(defgmethod
 (popup-menu+set-focused-item :class 'popup-menu :bind "set_focused_item" :hash
  1286410249)
 :void (index int))

(defgmethod
 (popup-menu+get-focused-item :class 'popup-menu :bind "get_focused_item" :hash
  3905245786)
 int)

(defgmethod
 (popup-menu+set-item-count :class 'popup-menu :bind "set_item_count" :hash
  1286410249)
 :void (count int))

(defgmethod
 (popup-menu+get-item-count :class 'popup-menu :bind "get_item_count" :hash
  3905245786)
 int)

(defgmethod
 (popup-menu+scroll-to-item :class 'popup-menu :bind "scroll_to_item" :hash
  1286410249)
 :void (index int))

(defgmethod
 (popup-menu+remove-item :class 'popup-menu :bind "remove_item" :hash
  1286410249)
 :void (index int))

(defgmethod
 (popup-menu+add-separator :class 'popup-menu :bind "add_separator" :hash
  2266703459)
 :void (label string) (id int))

(defgmethod (popup-menu+clear :class 'popup-menu :bind "clear" :hash 107499316)
 :void (free-submenus bool))

(defgmethod
 (popup-menu+set-hide-on-item-selection :class 'popup-menu :bind
  "set_hide_on_item_selection" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (popup-menu+is-hide-on-item-selection :class 'popup-menu :bind
  "is_hide_on_item_selection" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-hide-on-checkable-item-selection :class 'popup-menu :bind
  "set_hide_on_checkable_item_selection" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (popup-menu+is-hide-on-checkable-item-selection :class 'popup-menu :bind
  "is_hide_on_checkable_item_selection" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-hide-on-state-item-selection :class 'popup-menu :bind
  "set_hide_on_state_item_selection" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (popup-menu+is-hide-on-state-item-selection :class 'popup-menu :bind
  "is_hide_on_state_item_selection" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-submenu-popup-delay :class 'popup-menu :bind
  "set_submenu_popup_delay" :hash 373806689)
 :void (seconds float))

(defgmethod
 (popup-menu+get-submenu-popup-delay :class 'popup-menu :bind
  "get_submenu_popup_delay" :hash 1740695150)
 float)

(defgmethod
 (popup-menu+set-allow-search :class 'popup-menu :bind "set_allow_search" :hash
  2586408642)
 :void (allow bool))

(defgmethod
 (popup-menu+get-allow-search :class 'popup-menu :bind "get_allow_search" :hash
  36873697)
 bool)

(defgmethod
 (popup-menu+is-system-menu :class 'popup-menu :bind "is_system_menu" :hash
  36873697)
 bool)

(defgmethod
 (popup-menu+set-system-menu :class 'popup-menu :bind "set_system_menu" :hash
  600639674)
 :void (system-menu-id native-menu+system-menus))

(defgmethod
 (popup-menu+get-system-menu :class 'popup-menu :bind "get_system_menu" :hash
  1222557358)
 native-menu+system-menus)

(defgmethod
 (popup-menu+set-search-bar-enabled :class 'popup-menu :bind
  "set_search_bar_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (popup-menu+is-search-bar-enabled :class 'popup-menu :bind
  "is_search_bar_enabled" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-search-bar-min-item-count :class 'popup-menu :bind
  "set_search_bar_min_item_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (popup-menu+get-search-bar-min-item-count :class 'popup-menu :bind
  "get_search_bar_min_item_count" :hash 3905245786)
 int)

(defgmethod
 (popup-menu+set-search-bar-fuzzy-search-enabled :class 'popup-menu :bind
  "set_search_bar_fuzzy_search_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (popup-menu+is-search-bar-fuzzy-search-enabled :class 'popup-menu :bind
  "is_search_bar_fuzzy_search_enabled" :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-search-bar-fuzzy-search-max-misses :class 'popup-menu :bind
  "set_search_bar_fuzzy_search_max_misses" :hash 1286410249)
 :void (max-misses int))

(defgmethod
 (popup-menu+get-search-bar-fuzzy-search-max-misses :class 'popup-menu :bind
  "get_search_bar_fuzzy_search_max_misses" :hash 3905245786)
 int)

(defgmethod
 (popup-menu+set-shrink-height :class 'popup-menu :bind "set_shrink_height"
  :hash 2586408642)
 :void (shrink bool))

(defgmethod
 (popup-menu+get-shrink-height :class 'popup-menu :bind "get_shrink_height"
  :hash 36873697)
 bool)

(defgmethod
 (popup-menu+set-shrink-width :class 'popup-menu :bind "set_shrink_width" :hash
  2586408642)
 :void (shrink bool))

(defgmethod
 (popup-menu+get-shrink-width :class 'popup-menu :bind "get_shrink_width" :hash
  36873697)
 bool)