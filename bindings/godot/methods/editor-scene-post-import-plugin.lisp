(common-lisp:in-package :%godot)


(defgmethod
 (editor-scene-post-import-plugin+%get-internal-import-options :class
  'editor-scene-post-import-plugin :bind "_get_internal_import_options" :hash
  1286410249 :virtual common-lisp:t)
 :void (category int))

(defgmethod
 (editor-scene-post-import-plugin+%get-internal-option-visibility :class
  'editor-scene-post-import-plugin :bind "_get_internal_option_visibility"
  :hash 3811255416 :virtual common-lisp:t)
 variant (category int) (for-animation bool) (option string))

(defgmethod
 (editor-scene-post-import-plugin+%get-internal-option-update-view-required
  :class 'editor-scene-post-import-plugin :bind
  "_get_internal_option_update_view_required" :hash 3957349696 :virtual
  common-lisp:t)
 variant (category int) (option string))

(defgmethod
 (editor-scene-post-import-plugin+%internal-process :class
  'editor-scene-post-import-plugin :bind "_internal_process" :hash 3641982463
  :virtual common-lisp:t)
 :void (category int) (base-node node) (node node) (resource resource))

(defgmethod
 (editor-scene-post-import-plugin+%get-import-options :class
  'editor-scene-post-import-plugin :bind "_get_import_options" :hash 83702148
  :virtual common-lisp:t)
 :void (path string))

(defgmethod
 (editor-scene-post-import-plugin+%get-option-visibility :class
  'editor-scene-post-import-plugin :bind "_get_option_visibility" :hash
  298836892 :virtual common-lisp:t)
 variant (path string) (for-animation bool) (option string))

(defgmethod
 (editor-scene-post-import-plugin+%pre-process :class
  'editor-scene-post-import-plugin :bind "_pre_process" :hash 1078189570
  :virtual common-lisp:t)
 :void (scene node))

(defgmethod
 (editor-scene-post-import-plugin+%post-process :class
  'editor-scene-post-import-plugin :bind "_post_process" :hash 1078189570
  :virtual common-lisp:t)
 :void (scene node))

(defgmethod
 (editor-scene-post-import-plugin+get-option-value :class
  'editor-scene-post-import-plugin :bind "get_option_value" :hash 2760726917)
 variant (name string-name))

(defgmethod
 (editor-scene-post-import-plugin+add-import-option :class
  'editor-scene-post-import-plugin :bind "add_import_option" :hash 402577236)
 :void (name string) (value variant))

(defgmethod
 (editor-scene-post-import-plugin+add-import-option-advanced :class
  'editor-scene-post-import-plugin :bind "add_import_option_advanced" :hash
  3674075649)
 :void (type variant+type) (name string) (default-value variant)
 (hint property-hint) (hint-string string) (usage-flags int))