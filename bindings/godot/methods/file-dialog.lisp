(common-lisp:in-package :%godot)


(defgmethod
 (file-dialog+clear-filters :class 'file-dialog :bind "clear_filters" :hash
  3218959716)
 :void)

(defgmethod
 (file-dialog+add-filter :class 'file-dialog :bind "add_filter" :hash
  914921954)
 :void (filter string) (description string) (mime-type string))

(defgmethod
 (file-dialog+set-filters :class 'file-dialog :bind "set_filters" :hash
  4015028928)
 :void (filters packed-string-array))

(defgmethod
 (file-dialog+get-filters :class 'file-dialog :bind "get_filters" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (file-dialog+clear-filename-filter :class 'file-dialog :bind
  "clear_filename_filter" :hash 3218959716)
 :void)

(defgmethod
 (file-dialog+set-filename-filter :class 'file-dialog :bind
  "set_filename_filter" :hash 83702148)
 :void (filter string))

(defgmethod
 (file-dialog+get-filename-filter :class 'file-dialog :bind
  "get_filename_filter" :hash 201670096)
 string)

(defgmethod
 (file-dialog+get-option-name :class 'file-dialog :bind "get_option_name" :hash
  844755477)
 string (option int))

(defgmethod
 (file-dialog+get-option-values :class 'file-dialog :bind "get_option_values"
  :hash 647634434)
 packed-string-array (option int))

(defgmethod
 (file-dialog+get-option-default :class 'file-dialog :bind "get_option_default"
  :hash 923996154)
 int (option int))

(defgmethod
 (file-dialog+set-option-name :class 'file-dialog :bind "set_option_name" :hash
  501894301)
 :void (option int) (name string))

(defgmethod
 (file-dialog+set-option-values :class 'file-dialog :bind "set_option_values"
  :hash 3353661094)
 :void (option int) (values packed-string-array))

(defgmethod
 (file-dialog+set-option-default :class 'file-dialog :bind "set_option_default"
  :hash 3937882851)
 :void (option int) (default-value-index int))

(defgmethod
 (file-dialog+set-option-count :class 'file-dialog :bind "set_option_count"
  :hash 1286410249)
 :void (count int))

(defgmethod
 (file-dialog+get-option-count :class 'file-dialog :bind "get_option_count"
  :hash 3905245786)
 int)

(defgmethod
 (file-dialog+add-option :class 'file-dialog :bind "add_option" :hash
  149592325)
 :void (name string) (values packed-string-array) (default-value-index int))

(defgmethod
 (file-dialog+get-selected-options :class 'file-dialog :bind
  "get_selected_options" :hash 3102165223)
 dictionary)

(defgmethod
 (file-dialog+get-current-dir :class 'file-dialog :bind "get_current_dir" :hash
  201670096)
 string)

(defgmethod
 (file-dialog+get-current-file :class 'file-dialog :bind "get_current_file"
  :hash 201670096)
 string)

(defgmethod
 (file-dialog+get-current-path :class 'file-dialog :bind "get_current_path"
  :hash 201670096)
 string)

(defgmethod
 (file-dialog+set-current-dir :class 'file-dialog :bind "set_current_dir" :hash
  83702148)
 :void (dir string))

(defgmethod
 (file-dialog+set-current-file :class 'file-dialog :bind "set_current_file"
  :hash 83702148)
 :void (file string))

(defgmethod
 (file-dialog+set-current-path :class 'file-dialog :bind "set_current_path"
  :hash 83702148)
 :void (path string))

(defgmethod
 (file-dialog+set-mode-overrides-title :class 'file-dialog :bind
  "set_mode_overrides_title" :hash 2586408642)
 :void (override bool))

(defgmethod
 (file-dialog+is-mode-overriding-title :class 'file-dialog :bind
  "is_mode_overriding_title" :hash 36873697)
 bool)

(defgmethod
 (file-dialog+set-file-mode :class 'file-dialog :bind "set_file_mode" :hash
  3654936397)
 :void (mode file-dialog+file-mode))

(defgmethod
 (file-dialog+get-file-mode :class 'file-dialog :bind "get_file_mode" :hash
  4074825319)
 file-dialog+file-mode)

(defgmethod
 (file-dialog+set-display-mode :class 'file-dialog :bind "set_display_mode"
  :hash 2692197101)
 :void (mode file-dialog+display-mode))

(defgmethod
 (file-dialog+get-display-mode :class 'file-dialog :bind "get_display_mode"
  :hash 1092104624)
 file-dialog+display-mode)

(defgmethod
 (file-dialog+get-vbox :class 'file-dialog :bind "get_vbox" :hash 915758477)
 vbox-container)

(defgmethod
 (file-dialog+get-line-edit :class 'file-dialog :bind "get_line_edit" :hash
  4071694264)
 line-edit)

(defgmethod
 (file-dialog+set-access :class 'file-dialog :bind "set_access" :hash
  4104413466)
 :void (access file-dialog+access))

(defgmethod
 (file-dialog+get-access :class 'file-dialog :bind "get_access" :hash
  3344081076)
 file-dialog+access)

(defgmethod
 (file-dialog+set-root-subfolder :class 'file-dialog :bind "set_root_subfolder"
  :hash 83702148)
 :void (dir string))

(defgmethod
 (file-dialog+get-root-subfolder :class 'file-dialog :bind "get_root_subfolder"
  :hash 201670096)
 string)

(defgmethod
 (file-dialog+set-show-hidden-files :class 'file-dialog :bind
  "set_show_hidden_files" :hash 2586408642)
 :void (show bool))

(defgmethod
 (file-dialog+is-showing-hidden-files :class 'file-dialog :bind
  "is_showing_hidden_files" :hash 36873697)
 bool)

(defgmethod
 (file-dialog+set-use-native-dialog :class 'file-dialog :bind
  "set_use_native_dialog" :hash 2586408642)
 :void (native bool))

(defgmethod
 (file-dialog+get-use-native-dialog :class 'file-dialog :bind
  "get_use_native_dialog" :hash 36873697)
 bool)

(defgmethod
 (file-dialog+set-customization-flag-enabled :class 'file-dialog :bind
  "set_customization_flag_enabled" :hash 3849177100)
 :void (flag file-dialog+customization) (enabled bool))

(defgmethod
 (file-dialog+is-customization-flag-enabled :class 'file-dialog :bind
  "is_customization_flag_enabled" :hash 3722277863)
 bool (flag file-dialog+customization))

(defgmethod
 (file-dialog+deselect-all :class 'file-dialog :bind "deselect_all" :hash
  3218959716)
 :void)

(defgmethod
 (file-dialog+set-favorite-list :class 'file-dialog :bind "set_favorite_list"
  :hash 4015028928 :static common-lisp:t)
 :void (favorites packed-string-array))

(defgmethod
 (file-dialog+get-favorite-list :class 'file-dialog :bind "get_favorite_list"
  :hash 2981934095 :static common-lisp:t)
 packed-string-array)

(defgmethod
 (file-dialog+set-recent-list :class 'file-dialog :bind "set_recent_list" :hash
  4015028928 :static common-lisp:t)
 :void (recents packed-string-array))

(defgmethod
 (file-dialog+get-recent-list :class 'file-dialog :bind "get_recent_list" :hash
  2981934095 :static common-lisp:t)
 packed-string-array)

(defgmethod
 (file-dialog+set-get-icon-callback :class 'file-dialog :bind
  "set_get_icon_callback" :hash 1611583062 :static common-lisp:t)
 :void (callback callable))

(defgmethod
 (file-dialog+set-get-thumbnail-callback :class 'file-dialog :bind
  "set_get_thumbnail_callback" :hash 1611583062 :static common-lisp:t)
 :void (callback callable))

(defgmethod
 (file-dialog+popup-file-dialog :class 'file-dialog :bind "popup_file_dialog"
  :hash 3218959716)
 :void)

(defgmethod
 (file-dialog+invalidate :class 'file-dialog :bind "invalidate" :hash
  3218959716)
 :void)