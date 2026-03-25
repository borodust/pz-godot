(common-lisp:in-package :%godot)


(defgmethod
 (editor-export-preset+has :class 'editor-export-preset :bind "has" :hash
  2619796661)
 bool (property string-name))

(defgmethod
 (editor-export-preset+get-files-to-export :class 'editor-export-preset :bind
  "get_files_to_export" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-export-preset+get-customized-files :class 'editor-export-preset :bind
  "get_customized_files" :hash 3102165223)
 dictionary)

(defgmethod
 (editor-export-preset+get-customized-files-count :class 'editor-export-preset
  :bind "get_customized_files_count" :hash 3905245786)
 int)

(defgmethod
 (editor-export-preset+has-export-file :class 'editor-export-preset :bind
  "has_export_file" :hash 2323990056)
 bool (path string))

(defgmethod
 (editor-export-preset+get-file-export-mode :class 'editor-export-preset :bind
  "get_file_export_mode" :hash 407825436)
 editor-export-preset+file-export-mode (path string)
 (default editor-export-preset+file-export-mode))

(defgmethod
 (editor-export-preset+get-project-setting :class 'editor-export-preset :bind
  "get_project_setting" :hash 2138907829)
 variant (name string-name))

(defgmethod
 (editor-export-preset+get-preset-name :class 'editor-export-preset :bind
  "get_preset_name" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+is-runnable :class 'editor-export-preset :bind
  "is_runnable" :hash 36873697)
 bool)

(defgmethod
 (editor-export-preset+are-advanced-options-enabled :class
  'editor-export-preset :bind "are_advanced_options_enabled" :hash 36873697)
 bool)

(defgmethod
 (editor-export-preset+is-dedicated-server :class 'editor-export-preset :bind
  "is_dedicated_server" :hash 36873697)
 bool)

(defgmethod
 (editor-export-preset+get-export-filter :class 'editor-export-preset :bind
  "get_export_filter" :hash 4227045696)
 editor-export-preset+export-filter)

(defgmethod
 (editor-export-preset+get-include-filter :class 'editor-export-preset :bind
  "get_include_filter" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-exclude-filter :class 'editor-export-preset :bind
  "get_exclude_filter" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-custom-features :class 'editor-export-preset :bind
  "get_custom_features" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-patches :class 'editor-export-preset :bind
  "get_patches" :hash 1139954409)
 packed-string-array)

(defgmethod
 (editor-export-preset+get-export-path :class 'editor-export-preset :bind
  "get_export_path" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-encryption-in-filter :class 'editor-export-preset
  :bind "get_encryption_in_filter" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-encryption-ex-filter :class 'editor-export-preset
  :bind "get_encryption_ex_filter" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-encrypt-pck :class 'editor-export-preset :bind
  "get_encrypt_pck" :hash 36873697)
 bool)

(defgmethod
 (editor-export-preset+get-encrypt-directory :class 'editor-export-preset :bind
  "get_encrypt_directory" :hash 36873697)
 bool)

(defgmethod
 (editor-export-preset+get-encryption-key :class 'editor-export-preset :bind
  "get_encryption_key" :hash 201670096)
 string)

(defgmethod
 (editor-export-preset+get-script-export-mode :class 'editor-export-preset
  :bind "get_script_export_mode" :hash 2835358398)
 editor-export-preset+script-export-mode)

(defgmethod
 (editor-export-preset+get-or-env :class 'editor-export-preset :bind
  "get_or_env" :hash 389838787)
 variant (name string-name) (env-var string))

(defgmethod
 (editor-export-preset+get-version :class 'editor-export-preset :bind
  "get_version" :hash 1132184663)
 string (name string-name) (windows-version bool))