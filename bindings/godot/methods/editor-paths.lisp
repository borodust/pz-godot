(common-lisp:in-package :%godot)


(defgmethod
 (editor-paths+get-data-dir :class 'editor-paths :bind "get_data_dir" :hash
  201670096)
 string)

(defgmethod
 (editor-paths+get-config-dir :class 'editor-paths :bind "get_config_dir" :hash
  201670096)
 string)

(defgmethod
 (editor-paths+get-cache-dir :class 'editor-paths :bind "get_cache_dir" :hash
  201670096)
 string)

(defgmethod
 (editor-paths+is-self-contained :class 'editor-paths :bind "is_self_contained"
  :hash 36873697)
 bool)

(defgmethod
 (editor-paths+get-self-contained-file :class 'editor-paths :bind
  "get_self_contained_file" :hash 201670096)
 string)

(defgmethod
 (editor-paths+get-project-settings-dir :class 'editor-paths :bind
  "get_project_settings_dir" :hash 201670096)
 string)