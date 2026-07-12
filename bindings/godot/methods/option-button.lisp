(common-lisp:in-package :%godot)


(defgmethod
 (option-button+add-item :class 'option-button :bind "add_item" :hash
  2697778442)
 :void (label string) (id int))

(defgmethod
 (option-button+add-icon-item :class 'option-button :bind "add_icon_item" :hash
  3781678508)
 :void (texture texture-2d) (label string) (id int))

(defgmethod
 (option-button+set-item-text :class 'option-button :bind "set_item_text" :hash
  501894301)
 :void (idx int) (text string))

(defgmethod
 (option-button+set-item-icon :class 'option-button :bind "set_item_icon" :hash
  666127730)
 :void (idx int) (texture texture-2d))

(defgmethod
 (option-button+set-item-disabled :class 'option-button :bind
  "set_item_disabled" :hash 300928843)
 :void (idx int) (disabled bool))

(defgmethod
 (option-button+set-item-id :class 'option-button :bind "set_item_id" :hash
  3937882851)
 :void (idx int) (id int))

(defgmethod
 (option-button+set-item-metadata :class 'option-button :bind
  "set_item_metadata" :hash 2152698145)
 :void (idx int) (metadata variant))

(defgmethod
 (option-button+set-item-tooltip :class 'option-button :bind "set_item_tooltip"
  :hash 501894301)
 :void (idx int) (tooltip string))

(defgmethod
 (option-button+set-item-auto-translate-mode :class 'option-button :bind
  "set_item_auto_translate_mode" :hash 287402019)
 :void (idx int) (mode node+auto-translate-mode))

(defgmethod
 (option-button+set-search-bar-enabled :class 'option-button :bind
  "set_search_bar_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (option-button+set-search-bar-min-item-count :class 'option-button :bind
  "set_search_bar_min_item_count" :hash 1286410249)
 :void (count int))

(defgmethod
 (option-button+get-search-bar-min-item-count :class 'option-button :bind
  "get_search_bar_min_item_count" :hash 3905245786)
 int)

(defgmethod
 (option-button+set-search-bar-fuzzy-search-enabled :class 'option-button :bind
  "set_search_bar_fuzzy_search_enabled" :hash 2586408642)
 :void (enabled bool))

(defgmethod
 (option-button+is-search-bar-fuzzy-search-enabled :class 'option-button :bind
  "is_search_bar_fuzzy_search_enabled" :hash 36873697)
 bool)

(defgmethod
 (option-button+set-search-bar-fuzzy-search-max-misses :class 'option-button
  :bind "set_search_bar_fuzzy_search_max_misses" :hash 1286410249)
 :void (max-misses int))

(defgmethod
 (option-button+get-search-bar-fuzzy-search-max-misses :class 'option-button
  :bind "get_search_bar_fuzzy_search_max_misses" :hash 3905245786)
 int)

(defgmethod
 (option-button+get-item-text :class 'option-button :bind "get_item_text" :hash
  844755477)
 string (idx int))

(defgmethod
 (option-button+get-item-icon :class 'option-button :bind "get_item_icon" :hash
  3536238170)
 texture-2d (idx int))

(defgmethod
 (option-button+get-item-id :class 'option-button :bind "get_item_id" :hash
  923996154)
 int (idx int))

(defgmethod
 (option-button+get-item-index :class 'option-button :bind "get_item_index"
  :hash 923996154)
 int (id int))

(defgmethod
 (option-button+get-item-metadata :class 'option-button :bind
  "get_item_metadata" :hash 4227898402)
 variant (idx int))

(defgmethod
 (option-button+get-item-tooltip :class 'option-button :bind "get_item_tooltip"
  :hash 844755477)
 string (idx int))

(defgmethod
 (option-button+get-item-auto-translate-mode :class 'option-button :bind
  "get_item_auto_translate_mode" :hash 906302372)
 node+auto-translate-mode (idx int))

(defgmethod
 (option-button+is-item-disabled :class 'option-button :bind "is_item_disabled"
  :hash 1116898809)
 bool (idx int))

(defgmethod
 (option-button+is-item-separator :class 'option-button :bind
  "is_item_separator" :hash 1116898809)
 bool (idx int))

(defgmethod
 (option-button+is-search-bar-enabled :class 'option-button :bind
  "is_search_bar_enabled" :hash 36873697)
 bool)

(defgmethod
 (option-button+add-separator :class 'option-button :bind "add_separator" :hash
  3005725572)
 :void (text string))

(defgmethod
 (option-button+clear :class 'option-button :bind "clear" :hash 3218959716)
 :void)

(defgmethod
 (option-button+select :class 'option-button :bind "select" :hash 1286410249)
 :void (idx int))

(defgmethod
 (option-button+get-selected :class 'option-button :bind "get_selected" :hash
  3905245786)
 int)

(defgmethod
 (option-button+get-selected-id :class 'option-button :bind "get_selected_id"
  :hash 3905245786)
 int)

(defgmethod
 (option-button+get-selected-metadata :class 'option-button :bind
  "get_selected_metadata" :hash 1214101251)
 variant)

(defgmethod
 (option-button+remove-item :class 'option-button :bind "remove_item" :hash
  1286410249)
 :void (idx int))

(defgmethod
 (option-button+get-popup :class 'option-button :bind "get_popup" :hash
  229722558)
 popup-menu)

(defgmethod
 (option-button+show-popup :class 'option-button :bind "show_popup" :hash
  3218959716)
 :void)

(defgmethod
 (option-button+set-item-count :class 'option-button :bind "set_item_count"
  :hash 1286410249)
 :void (count int))

(defgmethod
 (option-button+get-item-count :class 'option-button :bind "get_item_count"
  :hash 3905245786)
 int)

(defgmethod
 (option-button+has-selectable-items :class 'option-button :bind
  "has_selectable_items" :hash 36873697)
 bool)

(defgmethod
 (option-button+get-selectable-item :class 'option-button :bind
  "get_selectable_item" :hash 894402480)
 int (from-last bool))

(defgmethod
 (option-button+set-fit-to-longest-item :class 'option-button :bind
  "set_fit_to_longest_item" :hash 2586408642)
 :void (fit bool))

(defgmethod
 (option-button+is-fit-to-longest-item :class 'option-button :bind
  "is_fit_to_longest_item" :hash 36873697)
 bool)

(defgmethod
 (option-button+set-allow-reselect :class 'option-button :bind
  "set_allow_reselect" :hash 2586408642)
 :void (allow bool))

(defgmethod
 (option-button+get-allow-reselect :class 'option-button :bind
  "get_allow_reselect" :hash 36873697)
 bool)

(defgmethod
 (option-button+set-disable-shortcuts :class 'option-button :bind
  "set_disable_shortcuts" :hash 2586408642)
 :void (disabled bool))