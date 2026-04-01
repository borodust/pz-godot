(common-lisp:in-package :%godot)


(defgmethod
 (script-language-extension+%get-name :class 'script-language-extension :bind
  "_get_name" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-language-extension+%init :class 'script-language-extension :bind
  "_init" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%get-type :class 'script-language-extension :bind
  "_get_type" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-language-extension+%get-extension :class 'script-language-extension
  :bind "_get_extension" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-language-extension+%finish :class 'script-language-extension :bind
  "_finish" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%get-reserved-words :class
  'script-language-extension :bind "_get_reserved_words" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (script-language-extension+%is-control-flow-keyword :class
  'script-language-extension :bind "_is_control_flow_keyword" :hash 3927539163
  :virtual common-lisp:t)
 bool (keyword string))

(defgmethod
 (script-language-extension+%get-comment-delimiters :class
  'script-language-extension :bind "_get_comment_delimiters" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (script-language-extension+%get-doc-comment-delimiters :class
  'script-language-extension :bind "_get_doc_comment_delimiters" :hash
  1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (script-language-extension+%get-string-delimiters :class
  'script-language-extension :bind "_get_string_delimiters" :hash 1139954409
  :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (script-language-extension+%make-template :class 'script-language-extension
  :bind "_make_template" :hash 3583744548 :virtual common-lisp:t)
 script (template string) (class-name string) (base-class-name string))

(defgmethod
 (script-language-extension+%get-built-in-templates :class
  'script-language-extension :bind "_get_built_in_templates" :hash 3147814860
  :virtual common-lisp:t)
 array (object string-name))

(defgmethod
 (script-language-extension+%is-using-templates :class
  'script-language-extension :bind "_is_using_templates" :hash 2240911060
  :virtual common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%validate :class 'script-language-extension :bind
  "_validate" :hash 1697887509 :virtual common-lisp:t)
 dictionary (script string) (path string) (validate-functions bool)
 (validate-errors bool) (validate-warnings bool) (validate-safe-lines bool))

(defgmethod
 (script-language-extension+%validate-path :class 'script-language-extension
  :bind "_validate_path" :hash 3135753539 :virtual common-lisp:t)
 string (path string))

(defgmethod
 (script-language-extension+%create-script :class 'script-language-extension
  :bind "_create_script" :hash 1981248198 :virtual common-lisp:t)
 object)

(defgmethod
 (script-language-extension+%has-named-classes :class
  'script-language-extension :bind "_has_named_classes" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%supports-builtin-mode :class
  'script-language-extension :bind "_supports_builtin_mode" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%supports-documentation :class
  'script-language-extension :bind "_supports_documentation" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%can-inherit-from-file :class
  'script-language-extension :bind "_can_inherit_from_file" :hash 36873697
  :virtual common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%find-function :class 'script-language-extension
  :bind "_find_function" :hash 2878152881 :virtual common-lisp:t)
 int (function string) (code string))

(defgmethod
 (script-language-extension+%make-function :class 'script-language-extension
  :bind "_make_function" :hash 1243061914 :virtual common-lisp:t)
 string (class-name string) (function-name string)
 (function-args packed-string-array))

(defgmethod
 (script-language-extension+%can-make-function :class
  'script-language-extension :bind "_can_make_function" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%open-in-external-editor :class
  'script-language-extension :bind "_open_in_external_editor" :hash 552845695
  :virtual common-lisp:t)
 error (script script) (line int) (column int))

(defgmethod
 (script-language-extension+%overrides-external-editor :class
  'script-language-extension :bind "_overrides_external_editor" :hash
  2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-language-extension+%preferred-file-name-casing :class
  'script-language-extension :bind "_preferred_file_name_casing" :hash
  2969522789 :virtual common-lisp:t)
 script-language+script-name-casing)

(defgmethod
 (script-language-extension+%complete-code :class 'script-language-extension
  :bind "_complete_code" :hash 950756616 :virtual common-lisp:t)
 dictionary (code string) (path string) (owner object))

(defgmethod
 (script-language-extension+%lookup-code :class 'script-language-extension
  :bind "_lookup_code" :hash 3143837309 :virtual common-lisp:t)
 dictionary (code string) (symbol string) (path string) (owner object))

(defgmethod
 (script-language-extension+%auto-indent-code :class 'script-language-extension
  :bind "_auto_indent_code" :hash 2531480354 :virtual common-lisp:t)
 string (code string) (from-line int) (to-line int))

(defgmethod
 (script-language-extension+%add-global-constant :class
  'script-language-extension :bind "_add_global_constant" :hash 3776071444
  :virtual common-lisp:t)
 :void (name string-name) (value variant))

(defgmethod
 (script-language-extension+%add-named-global-constant :class
  'script-language-extension :bind "_add_named_global_constant" :hash
  3776071444 :virtual common-lisp:t)
 :void (name string-name) (value variant))

(defgmethod
 (script-language-extension+%remove-named-global-constant :class
  'script-language-extension :bind "_remove_named_global_constant" :hash
  3304788590 :virtual common-lisp:t)
 :void (name string-name))

(defgmethod
 (script-language-extension+%thread-enter :class 'script-language-extension
  :bind "_thread_enter" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%thread-exit :class 'script-language-extension
  :bind "_thread_exit" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%debug-get-error :class 'script-language-extension
  :bind "_debug_get_error" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-language-extension+%debug-get-stack-level-count :class
  'script-language-extension :bind "_debug_get_stack_level_count" :hash
  3905245786 :virtual common-lisp:t)
 int)

(defgmethod
 (script-language-extension+%debug-get-stack-level-line :class
  'script-language-extension :bind "_debug_get_stack_level_line" :hash
  923996154 :virtual common-lisp:t)
 int (level int))

(defgmethod
 (script-language-extension+%debug-get-stack-level-function :class
  'script-language-extension :bind "_debug_get_stack_level_function" :hash
  844755477 :virtual common-lisp:t)
 string (level int))

(defgmethod
 (script-language-extension+%debug-get-stack-level-source :class
  'script-language-extension :bind "_debug_get_stack_level_source" :hash
  844755477 :virtual common-lisp:t)
 string (level int))

(defgmethod
 (script-language-extension+%debug-get-stack-level-locals :class
  'script-language-extension :bind "_debug_get_stack_level_locals" :hash
  335235777 :virtual common-lisp:t)
 dictionary (level int) (max-subitems int) (max-depth int))

(defgmethod
 (script-language-extension+%debug-get-stack-level-members :class
  'script-language-extension :bind "_debug_get_stack_level_members" :hash
  335235777 :virtual common-lisp:t)
 dictionary (level int) (max-subitems int) (max-depth int))

(defgmethod
 (script-language-extension+%debug-get-stack-level-instance :class
  'script-language-extension :bind "_debug_get_stack_level_instance" :hash
  3744713108 :virtual common-lisp:t)
 (:pointer :void) (level int))

(defgmethod
 (script-language-extension+%debug-get-globals :class
  'script-language-extension :bind "_debug_get_globals" :hash 4123630098
  :virtual common-lisp:t)
 dictionary (max-subitems int) (max-depth int))

(defgmethod
 (script-language-extension+%debug-parse-stack-level-expression :class
  'script-language-extension :bind "_debug_parse_stack_level_expression" :hash
  1135811067 :virtual common-lisp:t)
 string (level int) (expression string) (max-subitems int) (max-depth int))

(defgmethod
 (script-language-extension+%debug-get-current-stack-info :class
  'script-language-extension :bind "_debug_get_current_stack_info" :hash
  2915620761 :virtual common-lisp:t)
 array)

(defgmethod
 (script-language-extension+%reload-all-scripts :class
  'script-language-extension :bind "_reload_all_scripts" :hash 3218959716
  :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%reload-scripts :class 'script-language-extension
  :bind "_reload_scripts" :hash 3156113851 :virtual common-lisp:t)
 :void (scripts array) (soft-reload bool))

(defgmethod
 (script-language-extension+%reload-tool-script :class
  'script-language-extension :bind "_reload_tool_script" :hash 1957307671
  :virtual common-lisp:t)
 :void (script script) (soft-reload bool))

(defgmethod
 (script-language-extension+%get-recognized-extensions :class
  'script-language-extension :bind "_get_recognized_extensions" :hash
  1139954409 :virtual common-lisp:t)
 packed-string-array)

(defgmethod
 (script-language-extension+%get-public-functions :class
  'script-language-extension :bind "_get_public_functions" :hash 3995934104
  :virtual common-lisp:t)
 array)

(defgmethod
 (script-language-extension+%get-public-constants :class
  'script-language-extension :bind "_get_public_constants" :hash 3102165223
  :virtual common-lisp:t)
 dictionary)

(defgmethod
 (script-language-extension+%get-public-annotations :class
  'script-language-extension :bind "_get_public_annotations" :hash 3995934104
  :virtual common-lisp:t)
 array)

(defgmethod
 (script-language-extension+%profiling-start :class 'script-language-extension
  :bind "_profiling_start" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%profiling-stop :class 'script-language-extension
  :bind "_profiling_stop" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%profiling-set-save-native-calls :class
  'script-language-extension :bind "_profiling_set_save_native_calls" :hash
  2586408642 :virtual common-lisp:t)
 :void (enable bool))

(defgmethod
 (script-language-extension+%profiling-get-accumulated-data :class
  'script-language-extension :bind "_profiling_get_accumulated_data" :hash
  50157827 :virtual common-lisp:t)
 int (info-array (:pointer script-language-extension-profiling-info))
 (info-max int))

(defgmethod
 (script-language-extension+%profiling-get-frame-data :class
  'script-language-extension :bind "_profiling_get_frame_data" :hash 50157827
  :virtual common-lisp:t)
 int (info-array (:pointer script-language-extension-profiling-info))
 (info-max int))

(defgmethod
 (script-language-extension+%frame :class 'script-language-extension :bind
  "_frame" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-language-extension+%handles-global-class-type :class
  'script-language-extension :bind "_handles_global_class_type" :hash
  3927539163 :virtual common-lisp:t)
 bool (type string))

(defgmethod
 (script-language-extension+%get-global-class-name :class
  'script-language-extension :bind "_get_global_class_name" :hash 2248993622
  :virtual common-lisp:t)
 dictionary (path string))