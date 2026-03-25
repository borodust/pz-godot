(common-lisp:in-package :%godot)


(defgmethod
 (editor-import-plugin+-get-importer-name :class 'editor-import-plugin :bind
  "_get_importer_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-import-plugin+-get-visible-name :class 'editor-import-plugin :bind
  "_get_visible_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-import-plugin+-get-preset-count :class 'editor-import-plugin :bind
  "_get_preset_count" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (editor-import-plugin+-get-preset-name :class 'editor-import-plugin :bind
  "_get_preset_name" :hash 844755477 :virtual common-lisp:t)
 string (preset-index int))

(defgmethod
 (editor-import-plugin+-get-recognized-extensions :class 'editor-import-plugin
  :bind "_get_recognized_extensions" :hash 1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-import-plugin+-get-import-options :class 'editor-import-plugin :bind
  "_get_import_options" :hash 520498173 :virtual common-lisp:t)
 array (path string) (preset-index int))

(defgmethod
 (editor-import-plugin+-get-save-extension :class 'editor-import-plugin :bind
  "_get_save_extension" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-import-plugin+-get-resource-type :class 'editor-import-plugin :bind
  "_get_resource_type" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (editor-import-plugin+-get-priority :class 'editor-import-plugin :bind
  "_get_priority" :hash 1740695150 :virtual common-lisp:t)
 float)

(defgmethod
 (editor-import-plugin+-get-import-order :class 'editor-import-plugin :bind
  "_get_import_order" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (editor-import-plugin+-get-format-version :class 'editor-import-plugin :bind
  "_get_format_version" :hash 3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (editor-import-plugin+-get-option-visibility :class 'editor-import-plugin
  :bind "_get_option_visibility" :hash 240466755 :virtual common-lisp:t)
 bool (path string) (option-name string-name) (options dictionary))

(defgmethod
 (editor-import-plugin+-import :class 'editor-import-plugin :bind "_import"
  :hash 4108152118 :virtual common-lisp:t)
 error (source-file string) (save-path string) (options dictionary)
 (platform-variants array) (gen-files array))

(defgmethod
 (editor-import-plugin+-can-import-threaded :class 'editor-import-plugin :bind
  "_can_import_threaded" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (editor-import-plugin+append-import-external-resource :class
  'editor-import-plugin :bind "append_import_external_resource" :hash
  320493106)
 error (path string) (custom-options dictionary) (custom-importer string)
 (generator-parameters variant))