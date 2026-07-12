(common-lisp:in-package :%godot)


(defgmethod
 (script-extension+%editor-can-reload-from-file :class 'script-extension :bind
  "_editor_can_reload_from_file" :hash 2240911060 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%placeholder-erased :class 'script-extension :bind
  "_placeholder_erased" :hash 1286410249 :virtual common-lisp:t)
 :void (placeholder (:pointer :void)))

(defgmethod
 (script-extension+%can-instantiate :class 'script-extension :bind
  "_can_instantiate" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%get-base-script :class 'script-extension :bind
  "_get_base_script" :hash 278624046 :virtual common-lisp:t)
 script)

(defgmethod
 (script-extension+%get-global-name :class 'script-extension :bind
  "_get_global_name" :hash 2002593661 :virtual common-lisp:t)
 string-name)

(defgmethod
 (script-extension+%inherits-script :class 'script-extension :bind
  "_inherits_script" :hash 3669307804 :virtual common-lisp:t)
 bool (script script))

(defgmethod
 (script-extension+%get-instance-base-type :class 'script-extension :bind
  "_get_instance_base_type" :hash 2002593661 :virtual common-lisp:t)
 string-name)

(defgmethod
 (script-extension+%instance-create :class 'script-extension :bind
  "_instance_create" :hash 1107568780 :virtual common-lisp:t)
 (:pointer :void) (for-object object))

(defgmethod
 (script-extension+%placeholder-instance-create :class 'script-extension :bind
  "_placeholder_instance_create" :hash 1107568780 :virtual common-lisp:t)
 (:pointer :void) (for-object object))

(defgmethod
 (script-extension+%has-source-code :class 'script-extension :bind
  "_has_source_code" :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%get-source-code :class 'script-extension :bind
  "_get_source_code" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-extension+%set-source-code :class 'script-extension :bind
  "_set_source_code" :hash 83702148 :virtual common-lisp:t)
 :void (code string))

(defgmethod
 (script-extension+%reload :class 'script-extension :bind "_reload" :hash
  1413768114 :virtual common-lisp:t)
 error (keep-state bool))

(defgmethod
 (script-extension+%get-doc-class-name :class 'script-extension :bind
  "_get_doc_class_name" :hash 2002593661 :virtual common-lisp:t)
 string-name)

(defgmethod
 (script-extension+%get-documentation :class 'script-extension :bind
  "_get_documentation" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (script-extension+%get-class-icon-path :class 'script-extension :bind
  "_get_class_icon_path" :hash 201670096 :virtual common-lisp:t)
 string)

(defgmethod
 (script-extension+%has-method :class 'script-extension :bind "_has_method"
  :hash 2619796661 :virtual common-lisp:t)
 bool (method string-name))

(defgmethod
 (script-extension+%has-static-method :class 'script-extension :bind
  "_has_static_method" :hash 2619796661 :virtual common-lisp:t)
 bool (method string-name))

(defgmethod
 (script-extension+%get-script-method-argument-count :class 'script-extension
  :bind "_get_script_method_argument_count" :hash 2760726917 :virtual
  common-lisp:t)
 variant (method string-name))

(defgmethod
 (script-extension+%get-method-info :class 'script-extension :bind
  "_get_method_info" :hash 4028089122 :virtual common-lisp:t)
 dictionary (method string-name))

(defgmethod
 (script-extension+%is-tool :class 'script-extension :bind "_is_tool" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%is-valid :class 'script-extension :bind "_is_valid" :hash
  36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%is-abstract :class 'script-extension :bind "_is_abstract"
  :hash 36873697 :virtual common-lisp:t)
 bool)

(defgmethod
 (script-extension+%get-language :class 'script-extension :bind "_get_language"
  :hash 3096237657 :virtual common-lisp:t)
 script-language)

(defgmethod
 (script-extension+%has-script-signal :class 'script-extension :bind
  "_has_script_signal" :hash 2619796661 :virtual common-lisp:t)
 bool (signal string-name))

(defgmethod
 (script-extension+%get-script-signal-list :class 'script-extension :bind
  "_get_script_signal_list" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (script-extension+%has-property-default-value :class 'script-extension :bind
  "_has_property_default_value" :hash 2619796661 :virtual common-lisp:t)
 bool (property string-name))

(defgmethod
 (script-extension+%get-property-default-value :class 'script-extension :bind
  "_get_property_default_value" :hash 2760726917 :virtual common-lisp:t)
 variant (property string-name))

(defgmethod
 (script-extension+%update-exports :class 'script-extension :bind
  "_update_exports" :hash 3218959716 :virtual common-lisp:t)
 :void)

(defgmethod
 (script-extension+%get-script-method-list :class 'script-extension :bind
  "_get_script_method_list" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (script-extension+%get-script-property-list :class 'script-extension :bind
  "_get_script_property_list" :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (script-extension+%get-member-line :class 'script-extension :bind
  "_get_member_line" :hash 2458036349 :virtual common-lisp:t)
 int (member string-name))

(defgmethod
 (script-extension+%get-constants :class 'script-extension :bind
  "_get_constants" :hash 3102165223 :virtual common-lisp:t)
 dictionary)

(defgmethod
 (script-extension+%get-members :class 'script-extension :bind "_get_members"
  :hash 3995934104 :virtual common-lisp:t)
 array)

(defgmethod
 (script-extension+%is-placeholder-fallback-enabled :class 'script-extension
  :bind "_is_placeholder_fallback_enabled" :hash 36873697 :virtual
  common-lisp:t)
 bool)

(defgmethod
 (script-extension+%get-rpc-config :class 'script-extension :bind
  "_get_rpc_config" :hash 1214101251 :virtual common-lisp:t)
 variant)

(defgmethod
 (script-extension+%instance-has :class 'script-extension :bind "_instance_has"
  :hash 397768994 :virtual common-lisp:t)
 bool (object object))