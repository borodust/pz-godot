(common-lisp:in-package :%godot)


(defgmethod
 (display-server+has-feature :class 'display-server :bind "has_feature" :hash
  334065950)
 bool (feature display-server+feature))

(defgmethod
 (display-server+get-name :class 'display-server :bind "get_name" :hash
  201670096)
 string)

(defgmethod
 (display-server+help-set-search-callbacks :class 'display-server :bind
  "help_set_search_callbacks" :hash 1687350599)
 :void (search-callback callable) (action-callback callable))

(defgmethod
 (display-server+global-menu-set-popup-callbacks :class 'display-server :bind
  "global_menu_set_popup_callbacks" :hash 3893727526)
 :void (menu-root string) (open-callback callable) (close-callback callable))

(defgmethod
 (display-server+global-menu-add-submenu-item :class 'display-server :bind
  "global_menu_add_submenu_item" :hash 2828985934)
 int (menu-root string) (label string) (submenu string) (index int))

(defgmethod
 (display-server+global-menu-add-item :class 'display-server :bind
  "global_menu_add_item" :hash 3616842746)
 int (menu-root string) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-check-item :class 'display-server :bind
  "global_menu_add_check_item" :hash 3616842746)
 int (menu-root string) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-icon-item :class 'display-server :bind
  "global_menu_add_icon_item" :hash 3867083847)
 int (menu-root string) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-icon-check-item :class 'display-server :bind
  "global_menu_add_icon_check_item" :hash 3867083847)
 int (menu-root string) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-radio-check-item :class 'display-server :bind
  "global_menu_add_radio_check_item" :hash 3616842746)
 int (menu-root string) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-icon-radio-check-item :class 'display-server
  :bind "global_menu_add_icon_radio_check_item" :hash 3867083847)
 int (menu-root string) (icon texture-2d) (label string) (callback callable)
 (key-callback callable) (tag variant) (accelerator key) (index int))

(defgmethod
 (display-server+global-menu-add-multistate-item :class 'display-server :bind
  "global_menu_add_multistate_item" :hash 3297554655)
 int (menu-root string) (label string) (max-states int) (default-state int)
 (callback callable) (key-callback callable) (tag variant) (accelerator key)
 (index int))

(defgmethod
 (display-server+global-menu-add-separator :class 'display-server :bind
  "global_menu_add_separator" :hash 3214812433)
 int (menu-root string) (index int))

(defgmethod
 (display-server+global-menu-get-item-index-from-text :class 'display-server
  :bind "global_menu_get_item_index_from_text" :hash 2878152881)
 int (menu-root string) (text string))

(defgmethod
 (display-server+global-menu-get-item-index-from-tag :class 'display-server
  :bind "global_menu_get_item_index_from_tag" :hash 2941063483)
 int (menu-root string) (tag variant))

(defgmethod
 (display-server+global-menu-is-item-checked :class 'display-server :bind
  "global_menu_is_item_checked" :hash 3511468594)
 bool (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-is-item-checkable :class 'display-server :bind
  "global_menu_is_item_checkable" :hash 3511468594)
 bool (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-is-item-radio-checkable :class 'display-server
  :bind "global_menu_is_item_radio_checkable" :hash 3511468594)
 bool (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-callback :class 'display-server :bind
  "global_menu_get_item_callback" :hash 748666903)
 callable (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-key-callback :class 'display-server :bind
  "global_menu_get_item_key_callback" :hash 748666903)
 callable (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-tag :class 'display-server :bind
  "global_menu_get_item_tag" :hash 330672633)
 variant (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-text :class 'display-server :bind
  "global_menu_get_item_text" :hash 591067909)
 string (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-submenu :class 'display-server :bind
  "global_menu_get_item_submenu" :hash 591067909)
 string (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-accelerator :class 'display-server :bind
  "global_menu_get_item_accelerator" :hash 936065394)
 key (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-is-item-disabled :class 'display-server :bind
  "global_menu_is_item_disabled" :hash 3511468594)
 bool (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-is-item-hidden :class 'display-server :bind
  "global_menu_is_item_hidden" :hash 3511468594)
 bool (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-tooltip :class 'display-server :bind
  "global_menu_get_item_tooltip" :hash 591067909)
 string (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-state :class 'display-server :bind
  "global_menu_get_item_state" :hash 3422818498)
 int (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-max-states :class 'display-server :bind
  "global_menu_get_item_max_states" :hash 3422818498)
 int (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-icon :class 'display-server :bind
  "global_menu_get_item_icon" :hash 3591713183)
 texture-2d (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-get-item-indentation-level :class 'display-server
  :bind "global_menu_get_item_indentation_level" :hash 3422818498)
 int (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-set-item-checked :class 'display-server :bind
  "global_menu_set_item_checked" :hash 4108344793)
 :void (menu-root string) (idx int) (checked bool))

(defgmethod
 (display-server+global-menu-set-item-checkable :class 'display-server :bind
  "global_menu_set_item_checkable" :hash 4108344793)
 :void (menu-root string) (idx int) (checkable bool))

(defgmethod
 (display-server+global-menu-set-item-radio-checkable :class 'display-server
  :bind "global_menu_set_item_radio_checkable" :hash 4108344793)
 :void (menu-root string) (idx int) (checkable bool))

(defgmethod
 (display-server+global-menu-set-item-callback :class 'display-server :bind
  "global_menu_set_item_callback" :hash 3809915389)
 :void (menu-root string) (idx int) (callback callable))

(defgmethod
 (display-server+global-menu-set-item-hover-callbacks :class 'display-server
  :bind "global_menu_set_item_hover_callbacks" :hash 3809915389)
 :void (menu-root string) (idx int) (callback callable))

(defgmethod
 (display-server+global-menu-set-item-key-callback :class 'display-server :bind
  "global_menu_set_item_key_callback" :hash 3809915389)
 :void (menu-root string) (idx int) (key-callback callable))

(defgmethod
 (display-server+global-menu-set-item-tag :class 'display-server :bind
  "global_menu_set_item_tag" :hash 453659863)
 :void (menu-root string) (idx int) (tag variant))

(defgmethod
 (display-server+global-menu-set-item-text :class 'display-server :bind
  "global_menu_set_item_text" :hash 965966136)
 :void (menu-root string) (idx int) (text string))

(defgmethod
 (display-server+global-menu-set-item-submenu :class 'display-server :bind
  "global_menu_set_item_submenu" :hash 965966136)
 :void (menu-root string) (idx int) (submenu string))

(defgmethod
 (display-server+global-menu-set-item-accelerator :class 'display-server :bind
  "global_menu_set_item_accelerator" :hash 566943293)
 :void (menu-root string) (idx int) (keycode key))

(defgmethod
 (display-server+global-menu-set-item-disabled :class 'display-server :bind
  "global_menu_set_item_disabled" :hash 4108344793)
 :void (menu-root string) (idx int) (disabled bool))

(defgmethod
 (display-server+global-menu-set-item-hidden :class 'display-server :bind
  "global_menu_set_item_hidden" :hash 4108344793)
 :void (menu-root string) (idx int) (hidden bool))

(defgmethod
 (display-server+global-menu-set-item-tooltip :class 'display-server :bind
  "global_menu_set_item_tooltip" :hash 965966136)
 :void (menu-root string) (idx int) (tooltip string))

(defgmethod
 (display-server+global-menu-set-item-state :class 'display-server :bind
  "global_menu_set_item_state" :hash 3474840532)
 :void (menu-root string) (idx int) (state int))

(defgmethod
 (display-server+global-menu-set-item-max-states :class 'display-server :bind
  "global_menu_set_item_max_states" :hash 3474840532)
 :void (menu-root string) (idx int) (max-states int))

(defgmethod
 (display-server+global-menu-set-item-icon :class 'display-server :bind
  "global_menu_set_item_icon" :hash 3201338066)
 :void (menu-root string) (idx int) (icon texture-2d))

(defgmethod
 (display-server+global-menu-set-item-indentation-level :class 'display-server
  :bind "global_menu_set_item_indentation_level" :hash 3474840532)
 :void (menu-root string) (idx int) (level int))

(defgmethod
 (display-server+global-menu-get-item-count :class 'display-server :bind
  "global_menu_get_item_count" :hash 1321353865)
 int (menu-root string))

(defgmethod
 (display-server+global-menu-remove-item :class 'display-server :bind
  "global_menu_remove_item" :hash 2956805083)
 :void (menu-root string) (idx int))

(defgmethod
 (display-server+global-menu-clear :class 'display-server :bind
  "global_menu_clear" :hash 83702148)
 :void (menu-root string))

(defgmethod
 (display-server+global-menu-get-system-menu-roots :class 'display-server :bind
  "global_menu_get_system_menu_roots" :hash 3102165223)
 dictionary)

(defgmethod
 (display-server+tts-is-speaking :class 'display-server :bind "tts_is_speaking"
  :hash 36873697)
 bool)

(defgmethod
 (display-server+tts-is-paused :class 'display-server :bind "tts_is_paused"
  :hash 36873697)
 bool)

(defgmethod
 (display-server+tts-get-voices :class 'display-server :bind "tts_get_voices"
  :hash 3995934104)
 array)

(defgmethod
 (display-server+tts-get-voices-for-language :class 'display-server :bind
  "tts_get_voices_for_language" :hash 4291131558)
 packed-string-array (language string))

(defgmethod
 (display-server+tts-speak :class 'display-server :bind "tts_speak" :hash
  903992738)
 :void (text string) (voice string) (volume int) (pitch float) (rate float)
 (utterance-id int) (interrupt bool))

(defgmethod
 (display-server+tts-pause :class 'display-server :bind "tts_pause" :hash
  3218959716)
 :void)

(defgmethod
 (display-server+tts-resume :class 'display-server :bind "tts_resume" :hash
  3218959716)
 :void)

(defgmethod
 (display-server+tts-stop :class 'display-server :bind "tts_stop" :hash
  3218959716)
 :void)

(defgmethod
 (display-server+tts-set-utterance-callback :class 'display-server :bind
  "tts_set_utterance_callback" :hash 109679083)
 :void (event display-server+ttsutterance-event) (callable callable))

(defgmethod
 (display-server+is-dark-mode-supported :class 'display-server :bind
  "is_dark_mode_supported" :hash 36873697)
 bool)

(defgmethod
 (display-server+is-dark-mode :class 'display-server :bind "is_dark_mode" :hash
  36873697)
 bool)

(defgmethod
 (display-server+get-accent-color :class 'display-server :bind
  "get_accent_color" :hash 3444240500)
 color)

(defgmethod
 (display-server+get-base-color :class 'display-server :bind "get_base_color"
  :hash 3444240500)
 color)

(defgmethod
 (display-server+set-system-theme-change-callback :class 'display-server :bind
  "set_system_theme_change_callback" :hash 1611583062)
 :void (callable callable))

(defgmethod
 (display-server+mouse-set-mode :class 'display-server :bind "mouse_set_mode"
  :hash 348288463)
 :void (mouse-mode display-server+mouse-mode))

(defgmethod
 (display-server+mouse-get-mode :class 'display-server :bind "mouse_get_mode"
  :hash 1353961651)
 display-server+mouse-mode)

(defgmethod
 (display-server+warp-mouse :class 'display-server :bind "warp_mouse" :hash
  1130785943)
 :void (position vector-2i))

(defgmethod
 (display-server+mouse-get-position :class 'display-server :bind
  "mouse_get_position" :hash 3690982128)
 vector-2i)

(defgmethod
 (display-server+mouse-get-button-state :class 'display-server :bind
  "mouse_get_button_state" :hash 2512161324)
 mouse-button-mask)

(defgmethod
 (display-server+clipboard-set :class 'display-server :bind "clipboard_set"
  :hash 83702148)
 :void (clipboard string))

(defgmethod
 (display-server+clipboard-get :class 'display-server :bind "clipboard_get"
  :hash 201670096)
 string)

(defgmethod
 (display-server+clipboard-get-image :class 'display-server :bind
  "clipboard_get_image" :hash 4190603485)
 image)

(defgmethod
 (display-server+clipboard-has :class 'display-server :bind "clipboard_has"
  :hash 36873697)
 bool)

(defgmethod
 (display-server+clipboard-has-image :class 'display-server :bind
  "clipboard_has_image" :hash 36873697)
 bool)

(defgmethod
 (display-server+clipboard-set-primary :class 'display-server :bind
  "clipboard_set_primary" :hash 83702148)
 :void (clipboard-primary string))

(defgmethod
 (display-server+clipboard-get-primary :class 'display-server :bind
  "clipboard_get_primary" :hash 201670096)
 string)

(defgmethod
 (display-server+get-display-cutouts :class 'display-server :bind
  "get_display_cutouts" :hash 3995934104)
 array)

(defgmethod
 (display-server+get-display-safe-area :class 'display-server :bind
  "get_display_safe_area" :hash 410525958)
 rect-2i)

(defgmethod
 (display-server+get-screen-count :class 'display-server :bind
  "get_screen_count" :hash 3905245786)
 int)

(defgmethod
 (display-server+get-primary-screen :class 'display-server :bind
  "get_primary_screen" :hash 3905245786)
 int)

(defgmethod
 (display-server+get-keyboard-focus-screen :class 'display-server :bind
  "get_keyboard_focus_screen" :hash 3905245786)
 int)

(defgmethod
 (display-server+get-screen-from-rect :class 'display-server :bind
  "get_screen_from_rect" :hash 741354659)
 int (rect rect-2))

(defgmethod
 (display-server+screen-get-position :class 'display-server :bind
  "screen_get_position" :hash 1725937825)
 vector-2i (screen int))

(defgmethod
 (display-server+screen-get-size :class 'display-server :bind "screen_get_size"
  :hash 1725937825)
 vector-2i (screen int))

(defgmethod
 (display-server+screen-get-usable-rect :class 'display-server :bind
  "screen_get_usable_rect" :hash 2439012528)
 rect-2i (screen int))

(defgmethod
 (display-server+screen-get-dpi :class 'display-server :bind "screen_get_dpi"
  :hash 181039630)
 int (screen int))

(defgmethod
 (display-server+screen-get-scale :class 'display-server :bind
  "screen_get_scale" :hash 909105437)
 float (screen int))

(defgmethod
 (display-server+is-touchscreen-available :class 'display-server :bind
  "is_touchscreen_available" :hash 36873697)
 bool)

(defgmethod
 (display-server+screen-get-max-scale :class 'display-server :bind
  "screen_get_max_scale" :hash 1740695150)
 float)

(defgmethod
 (display-server+screen-get-refresh-rate :class 'display-server :bind
  "screen_get_refresh_rate" :hash 909105437)
 float (screen int))

(defgmethod
 (display-server+screen-get-pixel :class 'display-server :bind
  "screen_get_pixel" :hash 1532707496)
 color (position vector-2i))

(defgmethod
 (display-server+screen-get-image :class 'display-server :bind
  "screen_get_image" :hash 3813388802)
 image (screen int))

(defgmethod
 (display-server+screen-get-image-rect :class 'display-server :bind
  "screen_get_image_rect" :hash 2601441065)
 image (rect rect-2i))

(defgmethod
 (display-server+screen-set-orientation :class 'display-server :bind
  "screen_set_orientation" :hash 2211511631)
 :void (orientation display-server+screen-orientation) (screen int))

(defgmethod
 (display-server+screen-get-orientation :class 'display-server :bind
  "screen_get_orientation" :hash 133818562)
 display-server+screen-orientation (screen int))

(defgmethod
 (display-server+screen-set-keep-on :class 'display-server :bind
  "screen_set_keep_on" :hash 2586408642)
 :void (enable bool))

(defgmethod
 (display-server+screen-is-kept-on :class 'display-server :bind
  "screen_is_kept_on" :hash 36873697)
 bool)

(defgmethod
 (display-server+get-window-list :class 'display-server :bind "get_window_list"
  :hash 1930428628)
 packed-int-32array)

(defgmethod
 (display-server+get-window-at-screen-position :class 'display-server :bind
  "get_window_at_screen_position" :hash 2485466453)
 int (position vector-2i))

(defgmethod
 (display-server+window-get-native-handle :class 'display-server :bind
  "window_get_native_handle" :hash 1096425680)
 int (handle-type display-server+handle-type) (window-id int))

(defgmethod
 (display-server+window-get-active-popup :class 'display-server :bind
  "window_get_active_popup" :hash 3905245786)
 int)

(defgmethod
 (display-server+window-set-popup-safe-rect :class 'display-server :bind
  "window_set_popup_safe_rect" :hash 3317281434)
 :void (window int) (rect rect-2i))

(defgmethod
 (display-server+window-get-popup-safe-rect :class 'display-server :bind
  "window_get_popup_safe_rect" :hash 2161169500)
 rect-2i (window int))

(defgmethod
 (display-server+window-set-title :class 'display-server :bind
  "window_set_title" :hash 441246282)
 :void (title string) (window-id int))

(defgmethod
 (display-server+window-get-title-size :class 'display-server :bind
  "window_get_title_size" :hash 2925301799)
 vector-2i (title string) (window-id int))

(defgmethod
 (display-server+window-set-mouse-passthrough :class 'display-server :bind
  "window_set_mouse_passthrough" :hash 1993637420)
 :void (region packed-vector-2array) (window-id int))

(defgmethod
 (display-server+window-get-current-screen :class 'display-server :bind
  "window_get_current_screen" :hash 1591665591)
 int (window-id int))

(defgmethod
 (display-server+window-set-current-screen :class 'display-server :bind
  "window_set_current_screen" :hash 2230941749)
 :void (screen int) (window-id int))

(defgmethod
 (display-server+window-get-position :class 'display-server :bind
  "window_get_position" :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-get-position-with-decorations :class 'display-server
  :bind "window_get_position_with_decorations" :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-set-position :class 'display-server :bind
  "window_set_position" :hash 2019273902)
 :void (position vector-2i) (window-id int))

(defgmethod
 (display-server+window-get-size :class 'display-server :bind "window_get_size"
  :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-set-size :class 'display-server :bind "window_set_size"
  :hash 2019273902)
 :void (size vector-2i) (window-id int))

(defgmethod
 (display-server+window-set-rect-changed-callback :class 'display-server :bind
  "window_set_rect_changed_callback" :hash 1091192925)
 :void (callback callable) (window-id int))

(defgmethod
 (display-server+window-set-window-event-callback :class 'display-server :bind
  "window_set_window_event_callback" :hash 1091192925)
 :void (callback callable) (window-id int))

(defgmethod
 (display-server+window-set-input-event-callback :class 'display-server :bind
  "window_set_input_event_callback" :hash 1091192925)
 :void (callback callable) (window-id int))

(defgmethod
 (display-server+window-set-input-text-callback :class 'display-server :bind
  "window_set_input_text_callback" :hash 1091192925)
 :void (callback callable) (window-id int))

(defgmethod
 (display-server+window-set-drop-files-callback :class 'display-server :bind
  "window_set_drop_files_callback" :hash 1091192925)
 :void (callback callable) (window-id int))

(defgmethod
 (display-server+window-get-attached-instance-id :class 'display-server :bind
  "window_get_attached_instance_id" :hash 1591665591)
 int (window-id int))

(defgmethod
 (display-server+window-get-max-size :class 'display-server :bind
  "window_get_max_size" :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-set-max-size :class 'display-server :bind
  "window_set_max_size" :hash 2019273902)
 :void (max-size vector-2i) (window-id int))

(defgmethod
 (display-server+window-get-min-size :class 'display-server :bind
  "window_get_min_size" :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-set-min-size :class 'display-server :bind
  "window_set_min_size" :hash 2019273902)
 :void (min-size vector-2i) (window-id int))

(defgmethod
 (display-server+window-get-size-with-decorations :class 'display-server :bind
  "window_get_size_with_decorations" :hash 763922886)
 vector-2i (window-id int))

(defgmethod
 (display-server+window-get-mode :class 'display-server :bind "window_get_mode"
  :hash 2185728461)
 display-server+window-mode (window-id int))

(defgmethod
 (display-server+window-set-mode :class 'display-server :bind "window_set_mode"
  :hash 1319965401)
 :void (mode display-server+window-mode) (window-id int))

(defgmethod
 (display-server+window-set-flag :class 'display-server :bind "window_set_flag"
  :hash 254894155)
 :void (flag display-server+window-flags) (enabled bool) (window-id int))

(defgmethod
 (display-server+window-get-flag :class 'display-server :bind "window_get_flag"
  :hash 802816991)
 bool (flag display-server+window-flags) (window-id int))

(defgmethod
 (display-server+window-set-icon :class 'display-server :bind "window_set_icon"
  :hash 2457502155)
 :void (icon image) (window-id int))

(defgmethod
 (display-server+window-set-window-buttons-offset :class 'display-server :bind
  "window_set_window_buttons_offset" :hash 2019273902)
 :void (offset vector-2i) (window-id int))

(defgmethod
 (display-server+window-get-safe-title-margins :class 'display-server :bind
  "window_get_safe_title_margins" :hash 2295066620)
 vector-3i (window-id int))

(defgmethod
 (display-server+window-request-attention :class 'display-server :bind
  "window_request_attention" :hash 1995695955)
 :void (window-id int))

(defgmethod
 (display-server+window-set-taskbar-progress-value :class 'display-server :bind
  "window_set_taskbar_progress_value" :hash 3506631519)
 :void (value float) (window-id int))

(defgmethod
 (display-server+window-set-taskbar-progress-state :class 'display-server :bind
  "window_set_taskbar_progress_state" :hash 4119882768)
 :void (state display-server+progress-state) (window-id int))

(defgmethod
 (display-server+window-move-to-foreground :class 'display-server :bind
  "window_move_to_foreground" :hash 1995695955)
 :void (window-id int))

(defgmethod
 (display-server+window-is-focused :class 'display-server :bind
  "window_is_focused" :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-can-draw :class 'display-server :bind "window_can_draw"
  :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-set-transient :class 'display-server :bind
  "window_set_transient" :hash 3937882851)
 :void (window-id int) (parent-window-id int))

(defgmethod
 (display-server+window-set-exclusive :class 'display-server :bind
  "window_set_exclusive" :hash 300928843)
 :void (window-id int) (exclusive bool))

(defgmethod
 (display-server+window-set-ime-active :class 'display-server :bind
  "window_set_ime_active" :hash 1661950165)
 :void (active bool) (window-id int))

(defgmethod
 (display-server+window-set-ime-position :class 'display-server :bind
  "window_set_ime_position" :hash 2019273902)
 :void (position vector-2i) (window-id int))

(defgmethod
 (display-server+window-set-vsync-mode :class 'display-server :bind
  "window_set_vsync_mode" :hash 2179333492)
 :void (vsync-mode display-server+vsync-mode) (window-id int))

(defgmethod
 (display-server+window-get-vsync-mode :class 'display-server :bind
  "window_get_vsync_mode" :hash 578873795)
 display-server+vsync-mode (window-id int))

(defgmethod
 (display-server+window-is-hdr-output-supported :class 'display-server :bind
  "window_is_hdr_output_supported" :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-request-hdr-output :class 'display-server :bind
  "window_request_hdr_output" :hash 1661950165)
 :void (enable bool) (window-id int))

(defgmethod
 (display-server+window-is-hdr-output-requested :class 'display-server :bind
  "window_is_hdr_output_requested" :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-is-hdr-output-enabled :class 'display-server :bind
  "window_is_hdr_output_enabled" :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-set-hdr-output-reference-luminance :class
  'display-server :bind "window_set_hdr_output_reference_luminance" :hash
  3506631519)
 :void (reference-luminance float) (window-id int))

(defgmethod
 (display-server+window-get-hdr-output-reference-luminance :class
  'display-server :bind "window_get_hdr_output_reference_luminance" :hash
  218038398)
 float (window-id int))

(defgmethod
 (display-server+window-get-hdr-output-current-reference-luminance :class
  'display-server :bind "window_get_hdr_output_current_reference_luminance"
  :hash 218038398)
 float (window-id int))

(defgmethod
 (display-server+window-set-hdr-output-max-luminance :class 'display-server
  :bind "window_set_hdr_output_max_luminance" :hash 3506631519)
 :void (max-luminance float) (window-id int))

(defgmethod
 (display-server+window-get-hdr-output-max-luminance :class 'display-server
  :bind "window_get_hdr_output_max_luminance" :hash 218038398)
 float (window-id int))

(defgmethod
 (display-server+window-get-hdr-output-current-max-luminance :class
  'display-server :bind "window_get_hdr_output_current_max_luminance" :hash
  218038398)
 float (window-id int))

(defgmethod
 (display-server+window-get-output-max-linear-value :class 'display-server
  :bind "window_get_output_max_linear_value" :hash 218038398)
 float (window-id int))

(defgmethod
 (display-server+window-is-maximize-allowed :class 'display-server :bind
  "window_is_maximize_allowed" :hash 1051549951)
 bool (window-id int))

(defgmethod
 (display-server+window-maximize-on-title-dbl-click :class 'display-server
  :bind "window_maximize_on_title_dbl_click" :hash 36873697)
 bool)

(defgmethod
 (display-server+window-minimize-on-title-dbl-click :class 'display-server
  :bind "window_minimize_on_title_dbl_click" :hash 36873697)
 bool)

(defgmethod
 (display-server+window-start-drag :class 'display-server :bind
  "window_start_drag" :hash 1995695955)
 :void (window-id int))

(defgmethod
 (display-server+window-start-resize :class 'display-server :bind
  "window_start_resize" :hash 4009722312)
 :void (edge display-server+window-resize-edge) (window-id int))

(defgmethod
 (display-server+window-set-color :class 'display-server :bind
  "window_set_color" :hash 2920490490)
 :void (color color))

(defgmethod
 (display-server+accessibility-should-increase-contrast :class 'display-server
  :bind "accessibility_should_increase_contrast" :hash 3905245786)
 int)

(defgmethod
 (display-server+accessibility-should-reduce-animation :class 'display-server
  :bind "accessibility_should_reduce_animation" :hash 3905245786)
 int)

(defgmethod
 (display-server+accessibility-should-reduce-transparency :class
  'display-server :bind "accessibility_should_reduce_transparency" :hash
  3905245786)
 int)

(defgmethod
 (display-server+accessibility-screen-reader-active :class 'display-server
  :bind "accessibility_screen_reader_active" :hash 3905245786)
 int)

(defgmethod
 (display-server+accessibility-create-element :class 'display-server :bind
  "accessibility_create_element" :hash 2968347744)
 rid (window-id int) (role display-server+accessibility-role))

(defgmethod
 (display-server+accessibility-create-sub-element :class 'display-server :bind
  "accessibility_create_sub_element" :hash 1949948826)
 rid (parent-rid rid) (role display-server+accessibility-role) (insert-pos int))

(defgmethod
 (display-server+accessibility-create-sub-text-edit-elements :class
  'display-server :bind "accessibility_create_sub_text_edit_elements" :hash
  2702009895)
 rid (parent-rid rid) (shaped-text rid) (min-height float) (insert-pos int)
 (is-last-line bool))

(defgmethod
 (display-server+accessibility-has-element :class 'display-server :bind
  "accessibility_has_element" :hash 4155700596)
 bool (id rid))

(defgmethod
 (display-server+accessibility-free-element :class 'display-server :bind
  "accessibility_free_element" :hash 2722037293)
 :void (id rid))

(defgmethod
 (display-server+accessibility-element-set-meta :class 'display-server :bind
  "accessibility_element_set_meta" :hash 3175752987)
 :void (id rid) (meta variant))

(defgmethod
 (display-server+accessibility-element-get-meta :class 'display-server :bind
  "accessibility_element_get_meta" :hash 4171304767)
 variant (id rid))

(defgmethod
 (display-server+accessibility-set-window-rect :class 'display-server :bind
  "accessibility_set_window_rect" :hash 2386961724)
 :void (window-id int) (rect-out rect-2) (rect-in rect-2))

(defgmethod
 (display-server+accessibility-set-window-focused :class 'display-server :bind
  "accessibility_set_window_focused" :hash 300928843)
 :void (window-id int) (focused bool))

(defgmethod
 (display-server+accessibility-update-set-focus :class 'display-server :bind
  "accessibility_update_set_focus" :hash 2722037293)
 :void (id rid))

(defgmethod
 (display-server+accessibility-get-window-root :class 'display-server :bind
  "accessibility_get_window_root" :hash 495598643)
 rid (window-id int))

(defgmethod
 (display-server+accessibility-update-set-role :class 'display-server :bind
  "accessibility_update_set_role" :hash 3352768215)
 :void (id rid) (role display-server+accessibility-role))

(defgmethod
 (display-server+accessibility-update-set-name :class 'display-server :bind
  "accessibility_update_set_name" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (display-server+accessibility-update-set-extra-info :class 'display-server
  :bind "accessibility_update_set_extra_info" :hash 2726140452)
 :void (id rid) (name string))

(defgmethod
 (display-server+accessibility-update-set-description :class 'display-server
  :bind "accessibility_update_set_description" :hash 2726140452)
 :void (id rid) (description string))

(defgmethod
 (display-server+accessibility-update-set-value :class 'display-server :bind
  "accessibility_update_set_value" :hash 2726140452)
 :void (id rid) (value string))

(defgmethod
 (display-server+accessibility-update-set-tooltip :class 'display-server :bind
  "accessibility_update_set_tooltip" :hash 2726140452)
 :void (id rid) (tooltip string))

(defgmethod
 (display-server+accessibility-update-set-bounds :class 'display-server :bind
  "accessibility_update_set_bounds" :hash 1378122625)
 :void (id rid) (rect rect-2))

(defgmethod
 (display-server+accessibility-update-set-transform :class 'display-server
  :bind "accessibility_update_set_transform" :hash 1246044741)
 :void (id rid) (transform transform-2d))

(defgmethod
 (display-server+accessibility-update-add-child :class 'display-server :bind
  "accessibility_update_add_child" :hash 395945892)
 :void (id rid) (child-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-controls :class
  'display-server :bind "accessibility_update_add_related_controls" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-details :class
  'display-server :bind "accessibility_update_add_related_details" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-described-by :class
  'display-server :bind "accessibility_update_add_related_described_by" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-flow-to :class
  'display-server :bind "accessibility_update_add_related_flow_to" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-labeled-by :class
  'display-server :bind "accessibility_update_add_related_labeled_by" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-add-related-radio-group :class
  'display-server :bind "accessibility_update_add_related_radio_group" :hash
  395945892)
 :void (id rid) (related-id rid))

(defgmethod
 (display-server+accessibility-update-set-active-descendant :class
  'display-server :bind "accessibility_update_set_active_descendant" :hash
  395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (display-server+accessibility-update-set-next-on-line :class 'display-server
  :bind "accessibility_update_set_next_on_line" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (display-server+accessibility-update-set-previous-on-line :class
  'display-server :bind "accessibility_update_set_previous_on_line" :hash
  395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (display-server+accessibility-update-set-member-of :class 'display-server
  :bind "accessibility_update_set_member_of" :hash 395945892)
 :void (id rid) (group-id rid))

(defgmethod
 (display-server+accessibility-update-set-in-page-link-target :class
  'display-server :bind "accessibility_update_set_in_page_link_target" :hash
  395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (display-server+accessibility-update-set-error-message :class 'display-server
  :bind "accessibility_update_set_error_message" :hash 395945892)
 :void (id rid) (other-id rid))

(defgmethod
 (display-server+accessibility-update-set-live :class 'display-server :bind
  "accessibility_update_set_live" :hash 2683302212)
 :void (id rid) (live display-server+accessibility-live-mode))

(defgmethod
 (display-server+accessibility-update-add-action :class 'display-server :bind
  "accessibility_update_add_action" :hash 2898696987)
 :void (id rid) (action display-server+accessibility-action)
 (callable callable))

(defgmethod
 (display-server+accessibility-update-add-custom-action :class 'display-server
  :bind "accessibility_update_add_custom_action" :hash 4153150897)
 :void (id rid) (action-id int) (action-description string))

(defgmethod
 (display-server+accessibility-update-set-table-row-count :class
  'display-server :bind "accessibility_update_set_table_row_count" :hash
  3411492887)
 :void (id rid) (count int))

(defgmethod
 (display-server+accessibility-update-set-table-column-count :class
  'display-server :bind "accessibility_update_set_table_column_count" :hash
  3411492887)
 :void (id rid) (count int))

(defgmethod
 (display-server+accessibility-update-set-table-row-index :class
  'display-server :bind "accessibility_update_set_table_row_index" :hash
  3411492887)
 :void (id rid) (index int))

(defgmethod
 (display-server+accessibility-update-set-table-column-index :class
  'display-server :bind "accessibility_update_set_table_column_index" :hash
  3411492887)
 :void (id rid) (index int))

(defgmethod
 (display-server+accessibility-update-set-table-cell-position :class
  'display-server :bind "accessibility_update_set_table_cell_position" :hash
  4288446313)
 :void (id rid) (row-index int) (column-index int))

(defgmethod
 (display-server+accessibility-update-set-table-cell-span :class
  'display-server :bind "accessibility_update_set_table_cell_span" :hash
  4288446313)
 :void (id rid) (row-span int) (column-span int))

(defgmethod
 (display-server+accessibility-update-set-list-item-count :class
  'display-server :bind "accessibility_update_set_list_item_count" :hash
  3411492887)
 :void (id rid) (size int))

(defgmethod
 (display-server+accessibility-update-set-list-item-index :class
  'display-server :bind "accessibility_update_set_list_item_index" :hash
  3411492887)
 :void (id rid) (index int))

(defgmethod
 (display-server+accessibility-update-set-list-item-level :class
  'display-server :bind "accessibility_update_set_list_item_level" :hash
  3411492887)
 :void (id rid) (level int))

(defgmethod
 (display-server+accessibility-update-set-list-item-selected :class
  'display-server :bind "accessibility_update_set_list_item_selected" :hash
  1265174801)
 :void (id rid) (selected bool))

(defgmethod
 (display-server+accessibility-update-set-list-item-expanded :class
  'display-server :bind "accessibility_update_set_list_item_expanded" :hash
  1265174801)
 :void (id rid) (expanded bool))

(defgmethod
 (display-server+accessibility-update-set-popup-type :class 'display-server
  :bind "accessibility_update_set_popup_type" :hash 2040885448)
 :void (id rid) (popup display-server+accessibility-popup-type))

(defgmethod
 (display-server+accessibility-update-set-checked :class 'display-server :bind
  "accessibility_update_set_checked" :hash 1265174801)
 :void (id rid) (checekd bool))

(defgmethod
 (display-server+accessibility-update-set-num-value :class 'display-server
  :bind "accessibility_update_set_num_value" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (display-server+accessibility-update-set-num-range :class 'display-server
  :bind "accessibility_update_set_num_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (display-server+accessibility-update-set-num-step :class 'display-server :bind
  "accessibility_update_set_num_step" :hash 1794382983)
 :void (id rid) (step float))

(defgmethod
 (display-server+accessibility-update-set-num-jump :class 'display-server :bind
  "accessibility_update_set_num_jump" :hash 1794382983)
 :void (id rid) (jump float))

(defgmethod
 (display-server+accessibility-update-set-scroll-x :class 'display-server :bind
  "accessibility_update_set_scroll_x" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (display-server+accessibility-update-set-scroll-x-range :class 'display-server
  :bind "accessibility_update_set_scroll_x_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (display-server+accessibility-update-set-scroll-y :class 'display-server :bind
  "accessibility_update_set_scroll_y" :hash 1794382983)
 :void (id rid) (position float))

(defgmethod
 (display-server+accessibility-update-set-scroll-y-range :class 'display-server
  :bind "accessibility_update_set_scroll_y_range" :hash 2513314492)
 :void (id rid) (min float) (max float))

(defgmethod
 (display-server+accessibility-update-set-text-decorations :class
  'display-server :bind "accessibility_update_set_text_decorations" :hash
  1672422386)
 :void (id rid) (underline bool) (strikethrough bool) (overline bool))

(defgmethod
 (display-server+accessibility-update-set-text-align :class 'display-server
  :bind "accessibility_update_set_text_align" :hash 3725995085)
 :void (id rid) (align horizontal-alignment))

(defgmethod
 (display-server+accessibility-update-set-text-selection :class 'display-server
  :bind "accessibility_update_set_text_selection" :hash 3119144029)
 :void (id rid) (text-start-id rid) (start-char int) (text-end-id rid)
 (end-char int))

(defgmethod
 (display-server+accessibility-update-set-flag :class 'display-server :bind
  "accessibility_update_set_flag" :hash 3758675396)
 :void (id rid) (flag display-server+accessibility-flags) (value bool))

(defgmethod
 (display-server+accessibility-update-set-classname :class 'display-server
  :bind "accessibility_update_set_classname" :hash 2726140452)
 :void (id rid) (classname string))

(defgmethod
 (display-server+accessibility-update-set-placeholder :class 'display-server
  :bind "accessibility_update_set_placeholder" :hash 2726140452)
 :void (id rid) (placeholder string))

(defgmethod
 (display-server+accessibility-update-set-language :class 'display-server :bind
  "accessibility_update_set_language" :hash 2726140452)
 :void (id rid) (language string))

(defgmethod
 (display-server+accessibility-update-set-text-orientation :class
  'display-server :bind "accessibility_update_set_text_orientation" :hash
  1265174801)
 :void (id rid) (vertical bool))

(defgmethod
 (display-server+accessibility-update-set-list-orientation :class
  'display-server :bind "accessibility_update_set_list_orientation" :hash
  1265174801)
 :void (id rid) (vertical bool))

(defgmethod
 (display-server+accessibility-update-set-shortcut :class 'display-server :bind
  "accessibility_update_set_shortcut" :hash 2726140452)
 :void (id rid) (shortcut string))

(defgmethod
 (display-server+accessibility-update-set-url :class 'display-server :bind
  "accessibility_update_set_url" :hash 2726140452)
 :void (id rid) (url string))

(defgmethod
 (display-server+accessibility-update-set-role-description :class
  'display-server :bind "accessibility_update_set_role_description" :hash
  2726140452)
 :void (id rid) (description string))

(defgmethod
 (display-server+accessibility-update-set-state-description :class
  'display-server :bind "accessibility_update_set_state_description" :hash
  2726140452)
 :void (id rid) (description string))

(defgmethod
 (display-server+accessibility-update-set-color-value :class 'display-server
  :bind "accessibility_update_set_color_value" :hash 2948539648)
 :void (id rid) (color color))

(defgmethod
 (display-server+accessibility-update-set-background-color :class
  'display-server :bind "accessibility_update_set_background_color" :hash
  2948539648)
 :void (id rid) (color color))

(defgmethod
 (display-server+accessibility-update-set-foreground-color :class
  'display-server :bind "accessibility_update_set_foreground_color" :hash
  2948539648)
 :void (id rid) (color color))

(defgmethod
 (display-server+ime-get-selection :class 'display-server :bind
  "ime_get_selection" :hash 3690982128)
 vector-2i)

(defgmethod
 (display-server+ime-get-text :class 'display-server :bind "ime_get_text" :hash
  201670096)
 string)

(defgmethod
 (display-server+virtual-keyboard-show :class 'display-server :bind
  "virtual_keyboard_show" :hash 3042891259)
 :void (existing-text string) (position rect-2)
 (type display-server+virtual-keyboard-type) (max-length int)
 (cursor-start int) (cursor-end int))

(defgmethod
 (display-server+virtual-keyboard-hide :class 'display-server :bind
  "virtual_keyboard_hide" :hash 3218959716)
 :void)

(defgmethod
 (display-server+virtual-keyboard-get-height :class 'display-server :bind
  "virtual_keyboard_get_height" :hash 3905245786)
 int)

(defgmethod
 (display-server+has-hardware-keyboard :class 'display-server :bind
  "has_hardware_keyboard" :hash 36873697)
 bool)

(defgmethod
 (display-server+set-hardware-keyboard-connection-change-callback :class
  'display-server :bind "set_hardware_keyboard_connection_change_callback"
  :hash 1611583062)
 :void (callable callable))

(defgmethod
 (display-server+cursor-set-shape :class 'display-server :bind
  "cursor_set_shape" :hash 2026291549)
 :void (shape display-server+cursor-shape))

(defgmethod
 (display-server+cursor-get-shape :class 'display-server :bind
  "cursor_get_shape" :hash 1087724927)
 display-server+cursor-shape)

(defgmethod
 (display-server+cursor-set-custom-image :class 'display-server :bind
  "cursor_set_custom_image" :hash 1816663697)
 :void (cursor resource) (shape display-server+cursor-shape) (hotspot vector-2))

(defgmethod
 (display-server+get-swap-cancel-ok :class 'display-server :bind
  "get_swap_cancel_ok" :hash 2240911060)
 bool)

(defgmethod
 (display-server+enable-for-stealing-focus :class 'display-server :bind
  "enable_for_stealing_focus" :hash 1286410249)
 :void (process-id int))

(defgmethod
 (display-server+dialog-show :class 'display-server :bind "dialog_show" :hash
  4115553226)
 error (title string) (description string) (buttons packed-string-array)
 (callback callable))

(defgmethod
 (display-server+dialog-input-text :class 'display-server :bind
  "dialog_input_text" :hash 3088703427)
 error (title string) (description string) (existing-text string)
 (callback callable))

(defgmethod
 (display-server+file-dialog-show :class 'display-server :bind
  "file_dialog_show" :hash 1386825884)
 error (title string) (current-directory string) (filename string)
 (show-hidden bool) (mode display-server+file-dialog-mode)
 (filters packed-string-array) (callback callable) (parent-window-id int))

(defgmethod
 (display-server+file-dialog-with-options-show :class 'display-server :bind
  "file_dialog_with_options_show" :hash 1448789813)
 error (title string) (current-directory string) (root string)
 (filename string) (show-hidden bool) (mode display-server+file-dialog-mode)
 (filters packed-string-array) (options array) (callback callable)
 (parent-window-id int))

(defgmethod
 (display-server+beep :class 'display-server :bind "beep" :hash 4051624405)
 :void)

(defgmethod
 (display-server+keyboard-get-layout-count :class 'display-server :bind
  "keyboard_get_layout_count" :hash 3905245786)
 int)

(defgmethod
 (display-server+keyboard-get-current-layout :class 'display-server :bind
  "keyboard_get_current_layout" :hash 3905245786)
 int)

(defgmethod
 (display-server+keyboard-set-current-layout :class 'display-server :bind
  "keyboard_set_current_layout" :hash 1286410249)
 :void (index int))

(defgmethod
 (display-server+keyboard-get-layout-language :class 'display-server :bind
  "keyboard_get_layout_language" :hash 844755477)
 string (index int))

(defgmethod
 (display-server+keyboard-get-layout-name :class 'display-server :bind
  "keyboard_get_layout_name" :hash 844755477)
 string (index int))

(defgmethod
 (display-server+keyboard-get-keycode-from-physical :class 'display-server
  :bind "keyboard_get_keycode_from_physical" :hash 3447613187)
 key (keycode key))

(defgmethod
 (display-server+keyboard-get-label-from-physical :class 'display-server :bind
  "keyboard_get_label_from_physical" :hash 3447613187)
 key (keycode key))

(defgmethod
 (display-server+show-emoji-and-symbol-picker :class 'display-server :bind
  "show_emoji_and_symbol_picker" :hash 4051624405)
 :void)

(defgmethod
 (display-server+color-picker :class 'display-server :bind "color_picker" :hash
  151643214)
 bool (callback callable))

(defgmethod
 (display-server+process-events :class 'display-server :bind "process_events"
  :hash 3218959716)
 :void)

(defgmethod
 (display-server+force-process-and-drop-events :class 'display-server :bind
  "force_process_and_drop_events" :hash 3218959716)
 :void)

(defgmethod
 (display-server+set-native-icon :class 'display-server :bind "set_native_icon"
  :hash 83702148)
 :void (filename string))

(defgmethod
 (display-server+set-icon :class 'display-server :bind "set_icon" :hash
  532598488)
 :void (image image))

(defgmethod
 (display-server+create-status-indicator :class 'display-server :bind
  "create_status_indicator" :hash 1904285171)
 int (icon texture-2d) (tooltip string) (callback callable))

(defgmethod
 (display-server+status-indicator-set-icon :class 'display-server :bind
  "status_indicator_set_icon" :hash 666127730)
 :void (id int) (icon texture-2d))

(defgmethod
 (display-server+status-indicator-set-tooltip :class 'display-server :bind
  "status_indicator_set_tooltip" :hash 501894301)
 :void (id int) (tooltip string))

(defgmethod
 (display-server+status-indicator-set-menu :class 'display-server :bind
  "status_indicator_set_menu" :hash 4040184819)
 :void (id int) (menu-rid rid))

(defgmethod
 (display-server+status-indicator-set-callback :class 'display-server :bind
  "status_indicator_set_callback" :hash 957362965)
 :void (id int) (callback callable))

(defgmethod
 (display-server+status-indicator-get-rect :class 'display-server :bind
  "status_indicator_get_rect" :hash 3327874267)
 rect-2 (id int))

(defgmethod
 (display-server+delete-status-indicator :class 'display-server :bind
  "delete_status_indicator" :hash 1286410249)
 :void (id int))

(defgmethod
 (display-server+tablet-get-driver-count :class 'display-server :bind
  "tablet_get_driver_count" :hash 3905245786)
 int)

(defgmethod
 (display-server+tablet-get-driver-name :class 'display-server :bind
  "tablet_get_driver_name" :hash 844755477)
 string (idx int))

(defgmethod
 (display-server+tablet-get-current-driver :class 'display-server :bind
  "tablet_get_current_driver" :hash 201670096)
 string)

(defgmethod
 (display-server+tablet-set-current-driver :class 'display-server :bind
  "tablet_set_current_driver" :hash 83702148)
 :void (name string))

(defgmethod
 (display-server+is-window-transparency-available :class 'display-server :bind
  "is_window_transparency_available" :hash 36873697)
 bool)

(defgmethod
 (display-server+register-additional-output :class 'display-server :bind
  "register_additional_output" :hash 3975164845)
 :void (object object))

(defgmethod
 (display-server+unregister-additional-output :class 'display-server :bind
  "unregister_additional_output" :hash 3975164845)
 :void (object object))

(defgmethod
 (display-server+has-additional-outputs :class 'display-server :bind
  "has_additional_outputs" :hash 36873697)
 bool)

(defgmethod
 (display-server+is-in-pip-mode :class 'display-server :bind "is_in_pip_mode"
  :hash 1885608816)
 bool (window-id int))

(defgmethod
 (display-server+pip-mode-enter :class 'display-server :bind "pip_mode_enter"
  :hash 1995695955)
 :void (window-id int))

(defgmethod
 (display-server+pip-mode-set-aspect-ratio :class 'display-server :bind
  "pip_mode_set_aspect_ratio" :hash 3471927553)
 :void (numerator int) (denominator int) (window-id int))

(defgmethod
 (display-server+pip-mode-set-auto-enter-on-background :class 'display-server
  :bind "pip_mode_set_auto_enter_on_background" :hash 1661950165)
 :void (auto-enter-on-background bool) (window-id int))