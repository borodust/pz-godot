(common-lisp:in-package :%godot)


(defgmethod
 (editor-export-platform-extension+%get-preset-features :class
  'editor-export-platform-extension :bind "_get_preset_features" :hash
  1387456631 :virtual common-lisp:t)
 packed-string-array (preset editor-export-preset))

(defgmethod
 (editor-export-platform-extension+%is-executable :class
  'editor-export-platform-extension :bind "_is_executable" :hash 3927539163
  :virtual common-lisp:t)
 bool (path string))

(defgmethod
 (editor-export-platform-extension+%get-export-options :class
  'editor-export-platform-extension :bind "_get_export_options" :hash
  3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (editor-export-platform-extension+%should-update-export-options :class
  'editor-export-platform-extension :bind "_should_update_export_options" :hash
  2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-export-platform-extension+%get-export-option-visibility :class
  'editor-export-platform-extension :bind "_get_export_option_visibility" :hash
  969350244 :virtual common-lisp:t)
 bool (preset editor-export-preset) (option string))

(defgmethod
 (editor-export-platform-extension+%get-export-option-warning :class
  'editor-export-platform-extension :bind "_get_export_option_warning" :hash
  805886795 :virtual common-lisp:t)
 string (preset editor-export-preset) (option string-name))

(defgmethod
 (editor-export-platform-extension+%get-os-name :class
  'editor-export-platform-extension :bind "_get_os_name" :hash 201670096
  :virtual common-lisp:t)
 string)

(defgmethod
 (editor-export-platform-extension+%get-name :class
  'editor-export-platform-extension :bind "_get_name" :hash 201670096 :virtual
  common-lisp:t)
 string)

(defgmethod
 (editor-export-platform-extension+%get-logo :class
  'editor-export-platform-extension :bind "_get_logo" :hash 3635182373 :virtual
  common-lisp:t)
 texture-2d)

(defgmethod
 (editor-export-platform-extension+%poll-export :class
  'editor-export-platform-extension :bind "_poll_export" :hash 2240911060
  :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-export-platform-extension+%get-options-count :class
  'editor-export-platform-extension :bind "_get_options_count" :hash 3905245786
  :virtual common-lisp:t)
 int)

(defgmethod
 (editor-export-platform-extension+%get-options-tooltip :class
  'editor-export-platform-extension :bind "_get_options_tooltip" :hash
  201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-export-platform-extension+%get-option-icon :class
  'editor-export-platform-extension :bind "_get_option_icon" :hash 3536238170
  :virtual common-lisp:t)
 texture-2d (device int))

(defgmethod
 (editor-export-platform-extension+%get-option-label :class
  'editor-export-platform-extension :bind "_get_option_label" :hash 844755477
  :virtual common-lisp:t)
 string (device int))

(defgmethod
 (editor-export-platform-extension+%get-option-tooltip :class
  'editor-export-platform-extension :bind "_get_option_tooltip" :hash 844755477
  :virtual common-lisp:t)
 string (device int))

(defgmethod
 (editor-export-platform-extension+%get-device-architecture :class
  'editor-export-platform-extension :bind "_get_device_architecture" :hash
  844755477 :virtual common-lisp:t)
 string (device int))

(defgmethod
 (editor-export-platform-extension+%cleanup :class
  'editor-export-platform-extension :bind "_cleanup" :hash 3218959716 :virtual
  common-lisp:t)
 :void)

(defgmethod
 (editor-export-platform-extension+%run :class
  'editor-export-platform-extension :bind "_run" :hash 1726914928 :virtual
  common-lisp:t)
 error (preset editor-export-preset) (device int)
 (debug-flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%get-run-icon :class
  'editor-export-platform-extension :bind "_get_run_icon" :hash 3635182373
  :virtual common-lisp:t)
 texture-2d)

(defgmethod
 (editor-export-platform-extension+%can-export :class
  'editor-export-platform-extension :bind "_can_export" :hash 493961987
  :virtual common-lisp:t)
 bool (preset editor-export-preset) (debug bool))

(defgmethod
 (editor-export-platform-extension+%has-valid-export-configuration :class
  'editor-export-platform-extension :bind "_has_valid_export_configuration"
  :hash 493961987 :virtual common-lisp:t)
 bool (preset editor-export-preset) (debug bool))

(defgmethod
 (editor-export-platform-extension+%has-valid-project-configuration :class
  'editor-export-platform-extension :bind "_has_valid_project_configuration"
  :hash 3117166915 :virtual common-lisp:t)
 bool (preset editor-export-preset))

(defgmethod
 (editor-export-platform-extension+%get-binary-extensions :class
  'editor-export-platform-extension :bind "_get_binary_extensions" :hash
  1387456631 :virtual common-lisp:t)
 packed-string-array (preset editor-export-preset))

(defgmethod
 (editor-export-platform-extension+%export-project :class
  'editor-export-platform-extension :bind "_export_project" :hash 1328957260
  :virtual common-lisp:t)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%export-pack :class
  'editor-export-platform-extension :bind "_export_pack" :hash 1328957260
  :virtual common-lisp:t)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%export-zip :class
  'editor-export-platform-extension :bind "_export_zip" :hash 1328957260
  :virtual common-lisp:t)
 error (preset editor-export-preset) (debug bool) (path string)
 (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%export-pack-patch :class
  'editor-export-platform-extension :bind "_export_pack_patch" :hash 454765315
  :virtual common-lisp:t)
 error (preset editor-export-preset) (debug bool) (path string)
 (patches packed-string-array) (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%export-zip-patch :class
  'editor-export-platform-extension :bind "_export_zip_patch" :hash 454765315
  :virtual common-lisp:t)
 error (preset editor-export-preset) (debug bool) (path string)
 (patches packed-string-array) (flags editor-export-platform+debug-flags))

(defgmethod
 (editor-export-platform-extension+%get-platform-features :class
  'editor-export-platform-extension :bind "_get_platform_features" :hash
  1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-export-platform-extension+%get-debug-protocol :class
  'editor-export-platform-extension :bind "_get_debug_protocol" :hash 201670096
  :virtual common-lisp:t)
 string)

(defgmethod
 (editor-export-platform-extension+%initialize :class
  'editor-export-platform-extension :bind "_initialize" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (editor-export-platform-extension+set-config-error :class
  'editor-export-platform-extension :bind "set_config_error" :hash 3089850668)
 :void (error-text string))

(defgmethod
 (editor-export-platform-extension+get-config-error :class
  'editor-export-platform-extension :bind "get_config_error" :hash 201670096)
 string)

(defgmethod
 (editor-export-platform-extension+set-config-missing-templates :class
  'editor-export-platform-extension :bind "set_config_missing_templates" :hash
  1695273946)
 :void (missing-templates bool))

(defgmethod
 (editor-export-platform-extension+get-config-missing-templates :class
  'editor-export-platform-extension :bind "get_config_missing_templates" :hash
  36873697)
 bool)