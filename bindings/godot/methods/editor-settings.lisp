(common-lisp:in-package :%godot)


(defgmethod
 (editor-settings+has-setting :class 'editor-settings :bind "has_setting" :hash
  3927539163)
 bool (name string))

(defgmethod
 (editor-settings+set-setting :class 'editor-settings :bind "set_setting" :hash
  402577236)
 :void (name string) (value variant))

(defgmethod
 (editor-settings+get-setting :class 'editor-settings :bind "get_setting" :hash
  1868160156)
 variant (name string))

(defgmethod
 (editor-settings+erase :class 'editor-settings :bind "erase" :hash 83702148)
 :void (property string))

(defgmethod
 (editor-settings+set-initial-value :class 'editor-settings :bind
  "set_initial_value" :hash 1529169264)
 :void (name string-name) (value variant) (update-current bool))

(defgmethod
 (editor-settings+add-property-info :class 'editor-settings :bind
  "add_property_info" :hash 4155329257)
 :void (info dictionary))

(defgmethod
 (editor-settings+set-project-metadata :class 'editor-settings :bind
  "set_project_metadata" :hash 2504492430)
 :void (section string) (key string) (data variant))

(defgmethod
 (editor-settings+get-project-metadata :class 'editor-settings :bind
  "get_project_metadata" :hash 89809366)
 variant (section string) (key string) (default variant))

(defgmethod
 (editor-settings+set-favorites :class 'editor-settings :bind "set_favorites"
  :hash 4015028928)
 :void (dirs packed-string-array))

(defgmethod
 (editor-settings+get-favorites :class 'editor-settings :bind "get_favorites"
  :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-settings+set-recent-dirs :class 'editor-settings :bind
  "set_recent_dirs" :hash 4015028928)
 :void (dirs packed-string-array))

(defgmethod
 (editor-settings+get-recent-dirs :class 'editor-settings :bind
  "get_recent_dirs" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-settings+set-builtin-action-override :class 'editor-settings :bind
  "set_builtin_action_override" :hash 1209351045)
 :void (name string) (actions-list array))

(defgmethod
 (editor-settings+add-shortcut :class 'editor-settings :bind "add_shortcut"
  :hash 4124020929)
 :void (path string) (shortcut shortcut))

(defgmethod
 (editor-settings+remove-shortcut :class 'editor-settings :bind
  "remove_shortcut" :hash 83702148)
 :void (path string))

(defgmethod
 (editor-settings+is-shortcut :class 'editor-settings :bind "is_shortcut" :hash
  699917945)
 bool (path string) (event input-event))

(defgmethod
 (editor-settings+has-shortcut :class 'editor-settings :bind "has_shortcut"
  :hash 3927539163)
 bool (path string))

(defgmethod
 (editor-settings+get-shortcut :class 'editor-settings :bind "get_shortcut"
  :hash 1149070301)
 shortcut (path string))

(defgmethod
 (editor-settings+get-shortcut-list :class 'editor-settings :bind
  "get_shortcut_list" :hash 2981934095)
 packed-string-array)

(defgmethod
 (editor-settings+check-changed-settings-in-group :class 'editor-settings :bind
  "check_changed_settings_in_group" :hash 3927539163)
 bool (setting-prefix string))

(defgmethod
 (editor-settings+get-changed-settings :class 'editor-settings :bind
  "get_changed_settings" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-settings+mark-setting-changed :class 'editor-settings :bind
  "mark_setting_changed" :hash 83702148)
 :void (setting string))