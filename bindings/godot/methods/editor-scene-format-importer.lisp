(common-lisp:in-package :%godot)


(defgmethod
 (editor-scene-format-importer+%get-extensions :class
  'editor-scene-format-importer :bind "_get_extensions" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (editor-scene-format-importer+%import-scene :class
  'editor-scene-format-importer :bind "_import_scene" :hash 3749238728 :virtual
  common-lisp:t)
 object (path string) (flags int) (options dictionary))

(defgmethod
 (editor-scene-format-importer+%get-import-options :class
  'editor-scene-format-importer :bind "_get_import_options" :hash 83702148
  :virtual common-lisp:t)
 :void (path string))

(defgmethod
 (editor-scene-format-importer+%get-option-visibility :class
  'editor-scene-format-importer :bind "_get_option_visibility" :hash 298836892
  :virtual common-lisp:t)
 variant (path string) (for-animation bool) (option string))

(defgmethod
 (editor-scene-format-importer+add-import-option :class
  'editor-scene-format-importer :bind "add_import_option" :hash 402577236)
 :void (name string) (value variant))

(defgmethod
 (editor-scene-format-importer+add-import-option-advanced :class
  'editor-scene-format-importer :bind "add_import_option_advanced" :hash
  3674075649)
 :void (type variant+type) (name string) (default-value variant)
 (hint property-hint) (hint-string string) (usage-flags int))