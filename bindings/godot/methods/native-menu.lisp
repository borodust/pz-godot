(common-lisp:in-package :%godot)


(defgmethod
 (native-menu+has-feature :class 'native-menu :bind "has_feature" :hash
  1708975490)
 bool (feature native-menu+feature))

(defgmethod
 (native-menu+has-system-menu :class 'native-menu :bind "has_system_menu" :hash
  718213027)
 bool (menu-id native-menu+system-menus))

(defgmethod
 (native-menu+get-system-menu :class 'native-menu :bind "get_system_menu" :hash
  469707506)
 rid (menu-id native-menu+system-menus))

(defgmethod
 (native-menu+get-system-menu-name :class 'native-menu :bind
  "get_system_menu_name" :hash 1281499290)
 string (menu-id native-menu+system-menus))

(defgmethod
 (native-menu+get-system-menu-text :class 'native-menu :bind
  "get_system_menu_text" :hash 1281499290)
 string (menu-id native-menu+system-menus))

(defgmethod
 (native-menu+set-system-menu-text :class 'native-menu :bind
  "set_system_menu_text" :hash 3925225603)
 :void (menu-id native-menu+system-menus) (name string))

(defgmethod
 (native-menu+create-menu :class 'native-menu :bind "create_menu" :hash
  529393457)
 rid)

(defgmethod
 (native-menu+has-menu :class 'native-menu :bind "has_menu" :hash 4155700596)
 bool (rid rid))

(defgmethod
 (native-menu+free-menu :class 'native-menu :bind "free_menu" :hash 2722037293)
 :void (rid rid))

(defgmethod
 (native-menu+get-size :class 'native-menu :bind "get_size" :hash 2440833711)
 vector-2 (rid rid))

(defgmethod
 (native-menu+popup :class 'native-menu :bind "popup" :hash 2450610377) :void
 (rid rid) (position vector-2i))

(defgmethod
 (native-menu+set-interface-direction :class 'native-menu :bind
  "set_interface_direction" :hash 1265174801)
 :void (rid rid) (is-rtl bool))

(defgmethod
 (native-menu+set-popup-open-callback :class 'native-menu :bind
  "set_popup_open_callback" :hash 3379118538)
 :void (rid rid) (callback callable))

(defgmethod
 (native-menu+get-popup-open-callback :class 'native-menu :bind
  "get_popup_open_callback" :hash 3170603026)
 callable (rid rid))

(defgmethod
 (native-menu+set-popup-close-callback :class 'native-menu :bind
  "set_popup_close_callback" :hash 3379118538)
 :void (rid rid) (callback callable))

(defgmethod
 (native-menu+get-popup-close-callback :class 'native-menu :bind
  "get_popup_close_callback" :hash 3170603026)
 callable (rid rid))

(defgmethod
 (native-menu+set-minimum-width :class 'native-menu :bind "set_minimum_width"
  :hash 1794382983)
 :void (rid rid) (width float))

(defgmethod
 (native-menu+get-minimum-width :class 'native-menu :bind "get_minimum_width"
  :hash 866169185)
 float (rid rid))

(defgmethod
 (native-menu+is-opened :class 'native-menu :bind "is_opened" :hash 4155700596)
 bool (rid rid))

(defgmethod
 (native-menu+add-submenu-item :class 'native-menu :bind "add_submenu_item"
  :hash 1002030223)
 int (rid rid) (label string) (submenu-rid rid) (tag variant) (index int))

(defgmethod
 (native-menu+add-item :class 'native-menu :bind "add_item" :hash 980552939)
 int (rid rid) (label string) (callback callable) (key-callback callable)
 (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-check-item :class 'native-menu :bind "add_check_item" :hash
  980552939)
 int (rid rid) (label string) (callback callable) (key-callback callable)
 (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-icon-item :class 'native-menu :bind "add_icon_item" :hash
  1372188274)
 int (rid rid) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-icon-check-item :class 'native-menu :bind
  "add_icon_check_item" :hash 1372188274)
 int (rid rid) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-radio-check-item :class 'native-menu :bind
  "add_radio_check_item" :hash 980552939)
 int (rid rid) (label string) (callback callable) (key-callback callable)
 (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-icon-radio-check-item :class 'native-menu :bind
  "add_icon_radio_check_item" :hash 1372188274)
 int (rid rid) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (native-menu+add-multistate-item :class 'native-menu :bind
  "add_multistate_item" :hash 2674635658)
 int (rid rid) (label string) (max-states int) (default-state int)
 (callback callable) (key-callback callable) (tag variant) (accelerator key)
 (index int))

(defgmethod
 (native-menu+add-separator :class 'native-menu :bind "add_separator" :hash
  448810126)
 int (rid rid) (index int))

(defgmethod
 (native-menu+find-item-index-with-text :class 'native-menu :bind
  "find_item_index_with_text" :hash 1362438794)
 int (rid rid) (text string))

(defgmethod
 (native-menu+find-item-index-with-tag :class 'native-menu :bind
  "find_item_index_with_tag" :hash 1260085030)
 int (rid rid) (tag variant))

(defgmethod
 (native-menu+find-item-index-with-submenu :class 'native-menu :bind
  "find_item_index_with_submenu" :hash 893635918)
 int (rid rid) (submenu-rid rid))

(defgmethod
 (native-menu+is-item-checked :class 'native-menu :bind "is_item_checked" :hash
  3120086654)
 bool (rid rid) (idx int))

(defgmethod
 (native-menu+is-item-checkable :class 'native-menu :bind "is_item_checkable"
  :hash 3120086654)
 bool (rid rid) (idx int))

(defgmethod
 (native-menu+is-item-radio-checkable :class 'native-menu :bind
  "is_item_radio_checkable" :hash 3120086654)
 bool (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-callback :class 'native-menu :bind "get_item_callback"
  :hash 1639989698)
 callable (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-key-callback :class 'native-menu :bind
  "get_item_key_callback" :hash 1639989698)
 callable (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-tag :class 'native-menu :bind "get_item_tag" :hash
  4069510997)
 variant (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-text :class 'native-menu :bind "get_item_text" :hash
  1464764419)
 string (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-submenu :class 'native-menu :bind "get_item_submenu"
  :hash 1066463050)
 rid (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-accelerator :class 'native-menu :bind
  "get_item_accelerator" :hash 316800700)
 key (rid rid) (idx int))

(defgmethod
 (native-menu+is-item-disabled :class 'native-menu :bind "is_item_disabled"
  :hash 3120086654)
 bool (rid rid) (idx int))

(defgmethod
 (native-menu+is-item-hidden :class 'native-menu :bind "is_item_hidden" :hash
  3120086654)
 bool (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-tooltip :class 'native-menu :bind "get_item_tooltip"
  :hash 1464764419)
 string (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-state :class 'native-menu :bind "get_item_state" :hash
  1120910005)
 int (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-max-states :class 'native-menu :bind
  "get_item_max_states" :hash 1120910005)
 int (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-icon :class 'native-menu :bind "get_item_icon" :hash
  3391850701)
 texture-2d (rid rid) (idx int))

(defgmethod
 (native-menu+get-item-indentation-level :class 'native-menu :bind
  "get_item_indentation_level" :hash 1120910005)
 int (rid rid) (idx int))

(defgmethod
 (native-menu+set-item-checked :class 'native-menu :bind "set_item_checked"
  :hash 2658558584)
 :void (rid rid) (idx int) (checked bool))

(defgmethod
 (native-menu+set-item-checkable :class 'native-menu :bind "set_item_checkable"
  :hash 2658558584)
 :void (rid rid) (idx int) (checkable bool))

(defgmethod
 (native-menu+set-item-radio-checkable :class 'native-menu :bind
  "set_item_radio_checkable" :hash 2658558584)
 :void (rid rid) (idx int) (checkable bool))

(defgmethod
 (native-menu+set-item-callback :class 'native-menu :bind "set_item_callback"
  :hash 2779810226)
 :void (rid rid) (idx int) (callback callable))

(defgmethod
 (native-menu+set-item-hover-callbacks :class 'native-menu :bind
  "set_item_hover_callbacks" :hash 2779810226)
 :void (rid rid) (idx int) (callback callable))

(defgmethod
 (native-menu+set-item-key-callback :class 'native-menu :bind
  "set_item_key_callback" :hash 2779810226)
 :void (rid rid) (idx int) (key-callback callable))

(defgmethod
 (native-menu+set-item-tag :class 'native-menu :bind "set_item_tag" :hash
  2706844827)
 :void (rid rid) (idx int) (tag variant))

(defgmethod
 (native-menu+set-item-text :class 'native-menu :bind "set_item_text" :hash
  4153150897)
 :void (rid rid) (idx int) (text string))

(defgmethod
 (native-menu+set-item-submenu :class 'native-menu :bind "set_item_submenu"
  :hash 2310537182)
 :void (rid rid) (idx int) (submenu-rid rid))

(defgmethod
 (native-menu+set-item-accelerator :class 'native-menu :bind
  "set_item_accelerator" :hash 786300043)
 :void (rid rid) (idx int) (keycode key))

(defgmethod
 (native-menu+set-item-disabled :class 'native-menu :bind "set_item_disabled"
  :hash 2658558584)
 :void (rid rid) (idx int) (disabled bool))

(defgmethod
 (native-menu+set-item-hidden :class 'native-menu :bind "set_item_hidden" :hash
  2658558584)
 :void (rid rid) (idx int) (hidden bool))

(defgmethod
 (native-menu+set-item-tooltip :class 'native-menu :bind "set_item_tooltip"
  :hash 4153150897)
 :void (rid rid) (idx int) (tooltip string))

(defgmethod
 (native-menu+set-item-state :class 'native-menu :bind "set_item_state" :hash
  4288446313)
 :void (rid rid) (idx int) (state int))

(defgmethod
 (native-menu+set-item-max-states :class 'native-menu :bind
  "set_item_max_states" :hash 4288446313)
 :void (rid rid) (idx int) (max-states int))

(defgmethod
 (native-menu+set-item-icon :class 'native-menu :bind "set_item_icon" :hash
  1388763257)
 :void (rid rid) (idx int) (icon texture-2d))

(defgmethod
 (native-menu+set-item-indentation-level :class 'native-menu :bind
  "set_item_indentation_level" :hash 4288446313)
 :void (rid rid) (idx int) (level int))

(defgmethod
 (native-menu+set-item-index :class 'native-menu :bind "set_item_index" :hash
  23951185)
 int (rid rid) (idx int) (target-idx int))

(defgmethod
 (native-menu+get-item-count :class 'native-menu :bind "get_item_count" :hash
  2198884583)
 int (rid rid))

(defgmethod
 (native-menu+is-system-menu :class 'native-menu :bind "is_system_menu" :hash
  4155700596)
 bool (rid rid))

(defgmethod
 (native-menu+remove-item :class 'native-menu :bind "remove_item" :hash
  3411492887)
 :void (rid rid) (idx int))

(defgmethod
 (native-menu+clear :class 'native-menu :bind "clear" :hash 2722037293) :void
 (rid rid))