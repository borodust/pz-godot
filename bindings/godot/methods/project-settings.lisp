(common-lisp:in-package :%godot)


(defgmethod
 (project-settings+has-setting :class 'project-settings :bind "has_setting"
  :hash 3927539163)
 bool (name string))

(defgmethod
 (project-settings+set-setting :class 'project-settings :bind "set_setting"
  :hash 402577236)
 :void (name string) (value variant))

(defgmethod
 (project-settings+get-setting :class 'project-settings :bind "get_setting"
  :hash 223050753)
 variant (name string) (default-value variant))

(defgmethod
 (project-settings+get-setting-with-override :class 'project-settings :bind
  "get_setting_with_override" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (project-settings+get-global-class-list :class 'project-settings :bind
  "get_global_class_list" :hash 2915620761)
 array)

(defgmethod
 (project-settings+get-setting-with-override-and-custom-features :class
  'project-settings :bind "get_setting_with_override_and_custom_features" :hash
  2434817427)
 variant (name string-name) (features packed-string-array))

(defgmethod
 (project-settings+set-order :class 'project-settings :bind "set_order" :hash
  2956805083)
 :void (name string) (position int))

(defgmethod
 (project-settings+get-order :class 'project-settings :bind "get_order" :hash
  1321353865)
 int (name string))

(defgmethod
 (project-settings+set-initial-value :class 'project-settings :bind
  "set_initial_value" :hash 402577236)
 :void (name string) (value variant))

(defgmethod
 (project-settings+set-as-basic :class 'project-settings :bind "set_as_basic"
  :hash 2678287736)
 :void (name string) (basic bool))

(defgmethod
 (project-settings+set-as-internal :class 'project-settings :bind
  "set_as_internal" :hash 2678287736)
 :void (name string) (internal bool))

(defgmethod
 (project-settings+add-property-info :class 'project-settings :bind
  "add_property_info" :hash 4155329257)
 :void (hint dictionary))

(defgmethod
 (project-settings+set-restart-if-changed :class 'project-settings :bind
  "set_restart_if_changed" :hash 2678287736)
 :void (name string) (restart bool))

(defgmethod
 (project-settings+clear :class 'project-settings :bind "clear" :hash 83702148)
 :void (name string))

(defgmethod
 (project-settings+localize-path :class 'project-settings :bind "localize_path"
  :hash 3135753539)
 string (path string))

(defgmethod
 (project-settings+globalize-path :class 'project-settings :bind
  "globalize_path" :hash 3135753539)
 string (path string))

(defgmethod
 (project-settings+save :class 'project-settings :bind "save" :hash 166280745)
 error)

(defgmethod
 (project-settings+load-resource-pack :class 'project-settings :bind
  "load_resource_pack" :hash 708980503)
 bool (pack string) (replace-files bool) (offset int))

(defgmethod
 (project-settings+save-custom :class 'project-settings :bind "save_custom"
  :hash 166001499)
 error (file string))

(defgmethod
 (project-settings+get-changed-settings :class 'project-settings :bind
  "get_changed_settings" :hash 1139954409)
 packed-string-array)

(defgmethod
 (project-settings+check-changed-settings-in-group :class 'project-settings
  :bind "check_changed_settings_in_group" :hash 3927539163)
 bool (setting-prefix string))