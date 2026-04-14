(common-lisp:in-package :%gdext)


(defifun ("get_godot_version" get-godot-version) :void
 (r-godot-version (:pointer godot-version)))

(defifun ("get_godot_version2" get-godot-version2) :void
 (r-godot-version (:pointer godot-version-2)))

(defifun ("mem_alloc" mem-alloc) (:pointer :void) (p-bytes :size))

(defifun ("mem_realloc" mem-realloc) (:pointer :void) (p-ptr (:pointer :void))
 (p-bytes :size))

(defifun ("mem_free" mem-free) :void (p-ptr (:pointer :void)))

(defifun ("mem_alloc2" mem-alloc2) (:pointer :void) (p-bytes :size)
 (p-pad-align bool))

(defifun ("mem_realloc2" mem-realloc2) (:pointer :void)
 (p-ptr (:pointer :void)) (p-bytes :size) (p-pad-align bool))

(defifun ("mem_free2" mem-free2) :void (p-ptr (:pointer :void))
 (p-pad-align bool))

(defifun ("print_error" print-error) :void (p-description (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify bool))

(defifun ("print_error_with_message" print-error-with-message) :void
 (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify bool))

(defifun ("print_warning" print-warning) :void (p-description (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify bool))

(defifun ("print_warning_with_message" print-warning-with-message) :void
 (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify bool))

(defifun ("print_script_error" print-script-error) :void
 (p-description (:pointer :char)) (p-function (:pointer :char))
 (p-file (:pointer :char)) (p-line :int32) (p-editor-notify bool))

(defifun ("print_script_error_with_message" print-script-error-with-message)
 :void (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify bool))

(defifun ("get_native_struct_size" get-native-struct-size) :uint64
 (p-name const-string-name-ptr))

(defifun ("variant_new_copy" variant-new-copy) :void
 (r-dest uninitialized-variant-ptr) (p-src const-variant-ptr))

(defifun ("variant_new_nil" variant-new-nil) :void
 (r-dest uninitialized-variant-ptr))

(defifun ("variant_destroy" variant-destroy) :void (p-self variant-ptr))

(defifun ("variant_call" variant-call) :void (p-self variant-ptr)
 (p-method const-string-name-ptr) (p-args (:pointer const-variant-ptr))
 (p-argument-count int) (r-return uninitialized-variant-ptr)
 (r-error (:pointer call-error)))

(defifun ("variant_call_static" variant-call-static) :void
 (p-type variant-type) (p-method const-string-name-ptr)
 (p-args (:pointer const-variant-ptr)) (p-argument-count int)
 (r-return uninitialized-variant-ptr) (r-error (:pointer call-error)))

(defifun ("variant_evaluate" variant-evaluate) :void (p-op variant-operator)
 (p-a const-variant-ptr) (p-b const-variant-ptr)
 (r-return uninitialized-variant-ptr) (r-valid (:pointer bool)))

(defifun ("variant_set" variant-set) :void (p-self variant-ptr)
 (p-key const-variant-ptr) (p-value const-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_set_named" variant-set-named) :void (p-self variant-ptr)
 (p-key const-string-name-ptr) (p-value const-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_set_keyed" variant-set-keyed) :void (p-self variant-ptr)
 (p-key const-variant-ptr) (p-value const-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_set_indexed" variant-set-indexed) :void (p-self variant-ptr)
 (p-index int) (p-value const-variant-ptr) (r-valid (:pointer bool))
 (r-oob (:pointer bool)))

(defifun ("variant_get" variant-get) :void (p-self const-variant-ptr)
 (p-key const-variant-ptr) (r-ret uninitialized-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_get_named" variant-get-named) :void
 (p-self const-variant-ptr) (p-key const-string-name-ptr)
 (r-ret uninitialized-variant-ptr) (r-valid (:pointer bool)))

(defifun ("variant_get_keyed" variant-get-keyed) :void
 (p-self const-variant-ptr) (p-key const-variant-ptr)
 (r-ret uninitialized-variant-ptr) (r-valid (:pointer bool)))

(defifun ("variant_get_indexed" variant-get-indexed) :void
 (p-self const-variant-ptr) (p-index int) (r-ret uninitialized-variant-ptr)
 (r-valid (:pointer bool)) (r-oob (:pointer bool)))

(defifun ("variant_iter_init" variant-iter-init) bool
 (p-self const-variant-ptr) (r-iter uninitialized-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_iter_next" variant-iter-next) bool
 (p-self const-variant-ptr) (r-iter variant-ptr) (r-valid (:pointer bool)))

(defifun ("variant_iter_get" variant-iter-get) :void (p-self const-variant-ptr)
 (r-iter variant-ptr) (r-ret uninitialized-variant-ptr)
 (r-valid (:pointer bool)))

(defifun ("variant_hash" variant-hash) int (p-self const-variant-ptr))

(defifun ("variant_recursive_hash" variant-recursive-hash) int
 (p-self const-variant-ptr) (p-recursion-count int))

(defifun ("variant_hash_compare" variant-hash-compare) bool
 (p-self const-variant-ptr) (p-other const-variant-ptr))

(defifun ("variant_booleanize" variant-booleanize) bool
 (p-self const-variant-ptr))

(defifun ("variant_duplicate" variant-duplicate) :void
 (p-self const-variant-ptr) (r-ret variant-ptr) (p-deep bool))

(defifun ("variant_stringify" variant-stringify) :void
 (p-self const-variant-ptr) (r-ret string-ptr))

(defifun ("variant_get_type" variant-get-type) variant-type
 (p-self const-variant-ptr))

(defifun ("variant_has_method" variant-has-method) bool
 (p-self const-variant-ptr) (p-method const-string-name-ptr))

(defifun ("variant_has_member" variant-has-member) bool (p-type variant-type)
 (p-member const-string-name-ptr))

(defifun ("variant_has_key" variant-has-key) bool (p-self const-variant-ptr)
 (p-key const-variant-ptr) (r-valid (:pointer bool)))

(defifun ("variant_get_object_instance_id" variant-get-object-instance-id)
 instance-id (p-self const-variant-ptr))

(defifun ("variant_get_type_name" variant-get-type-name) :void
 (p-type variant-type) (r-name uninitialized-string-ptr))

(defifun ("variant_can_convert" variant-can-convert) bool (p-from variant-type)
 (p-to variant-type))

(defifun ("variant_can_convert_strict" variant-can-convert-strict) bool
 (p-from variant-type) (p-to variant-type))

(defifun
 ("get_variant_from_type_constructor" get-variant-from-type-constructor)
 variant-from-type-constructor-func (p-type variant-type))

(defifun ("get_variant_to_type_constructor" get-variant-to-type-constructor)
 type-from-variant-constructor-func (p-type variant-type))

(defifun ("variant_get_ptr_internal_getter" variant-get-ptr-internal-getter)
 variant-get-internal-ptr-func (p-type variant-type))

(defifun
 ("variant_get_ptr_operator_evaluator" variant-get-ptr-operator-evaluator)
 ptr-operator-evaluator (p-operator variant-operator) (p-type-a variant-type)
 (p-type-b variant-type))

(defifun ("variant_get_ptr_builtin_method" variant-get-ptr-builtin-method)
 ptr-built-in-method (p-type variant-type) (p-method const-string-name-ptr)
 (p-hash int))

(defifun ("variant_get_ptr_constructor" variant-get-ptr-constructor)
 ptr-constructor (p-type variant-type) (p-constructor :int32))

(defifun ("variant_get_ptr_destructor" variant-get-ptr-destructor)
 ptr-destructor (p-type variant-type))

(defifun ("variant_construct" variant-construct) :void (p-type variant-type)
 (r-base uninitialized-variant-ptr) (p-args (:pointer const-variant-ptr))
 (p-argument-count :int32) (r-error (:pointer call-error)))

(defifun ("variant_get_ptr_setter" variant-get-ptr-setter) ptr-setter
 (p-type variant-type) (p-member const-string-name-ptr))

(defifun ("variant_get_ptr_getter" variant-get-ptr-getter) ptr-getter
 (p-type variant-type) (p-member const-string-name-ptr))

(defifun ("variant_get_ptr_indexed_setter" variant-get-ptr-indexed-setter)
 ptr-indexed-setter (p-type variant-type))

(defifun ("variant_get_ptr_indexed_getter" variant-get-ptr-indexed-getter)
 ptr-indexed-getter (p-type variant-type))

(defifun ("variant_get_ptr_keyed_setter" variant-get-ptr-keyed-setter)
 ptr-keyed-setter (p-type variant-type))

(defifun ("variant_get_ptr_keyed_getter" variant-get-ptr-keyed-getter)
 ptr-keyed-getter (p-type variant-type))

(defifun ("variant_get_ptr_keyed_checker" variant-get-ptr-keyed-checker)
 ptr-keyed-checker (p-type variant-type))

(defifun ("variant_get_constant_value" variant-get-constant-value) :void
 (p-type variant-type) (p-constant const-string-name-ptr)
 (r-ret uninitialized-variant-ptr))

(defifun ("variant_get_ptr_utility_function" variant-get-ptr-utility-function)
 ptr-utility-function (p-function const-string-name-ptr) (p-hash int))

(defifun ("string_new_with_latin1_chars" string-new-with-latin1-chars) :void
 (r-dest uninitialized-string-ptr) (p-contents (:pointer :char)))

(defifun ("string_new_with_utf8_chars" string-new-with-utf8-chars) :void
 (r-dest uninitialized-string-ptr) (p-contents (:pointer :char)))

(defifun ("string_new_with_utf16_chars" string-new-with-utf16-chars) :void
 (r-dest uninitialized-string-ptr) (p-contents (:pointer :uint16)))

(defifun ("string_new_with_utf32_chars" string-new-with-utf32-chars) :void
 (r-dest uninitialized-string-ptr) (p-contents (:pointer :uint32)))

(defifun ("string_new_with_wide_chars" string-new-with-wide-chars) :void
 (r-dest uninitialized-string-ptr) (p-contents (:pointer %gdext.util:wchar)))

(defifun
 ("string_new_with_latin1_chars_and_len" string-new-with-latin1-chars-and-len)
 :void (r-dest uninitialized-string-ptr) (p-contents (:pointer :char))
 (p-size int))

(defifun
 ("string_new_with_utf8_chars_and_len" string-new-with-utf8-chars-and-len)
 :void (r-dest uninitialized-string-ptr) (p-contents (:pointer :char))
 (p-size int))

(defifun
 ("string_new_with_utf8_chars_and_len2" string-new-with-utf8-chars-and-len2)
 int (r-dest uninitialized-string-ptr) (p-contents (:pointer :char))
 (p-size int))

(defifun
 ("string_new_with_utf16_chars_and_len" string-new-with-utf16-chars-and-len)
 :void (r-dest uninitialized-string-ptr) (p-contents (:pointer :uint16))
 (p-char-count int))

(defifun
 ("string_new_with_utf16_chars_and_len2" string-new-with-utf16-chars-and-len2)
 int (r-dest uninitialized-string-ptr) (p-contents (:pointer :uint16))
 (p-char-count int) (p-default-little-endian bool))

(defifun
 ("string_new_with_utf32_chars_and_len" string-new-with-utf32-chars-and-len)
 :void (r-dest uninitialized-string-ptr) (p-contents (:pointer :uint32))
 (p-char-count int))

(defifun
 ("string_new_with_wide_chars_and_len" string-new-with-wide-chars-and-len)
 :void (r-dest uninitialized-string-ptr)
 (p-contents (:pointer %gdext.util:wchar)) (p-char-count int))

(defifun ("string_to_latin1_chars" string-to-latin1-chars) int
 (p-self const-string-ptr) (r-text (:pointer :char)) (p-max-write-length int))

(defifun ("string_to_utf8_chars" string-to-utf8-chars) int
 (p-self const-string-ptr) (r-text (:pointer :char)) (p-max-write-length int))

(defifun ("string_to_utf16_chars" string-to-utf16-chars) int
 (p-self const-string-ptr) (r-text (:pointer :uint16)) (p-max-write-length int))

(defifun ("string_to_utf32_chars" string-to-utf32-chars) int
 (p-self const-string-ptr) (r-text (:pointer :uint32)) (p-max-write-length int))

(defifun ("string_to_wide_chars" string-to-wide-chars) int
 (p-self const-string-ptr) (r-text (:pointer %gdext.util:wchar))
 (p-max-write-length int))

(defifun ("string_operator_index" string-operator-index) (:pointer :uint32)
 (p-self string-ptr) (p-index int))

(defifun ("string_operator_index_const" string-operator-index-const)
 (:pointer :uint32) (p-self const-string-ptr) (p-index int))

(defifun ("string_operator_plus_eq_string" string-operator-plus-eq-string)
 :void (p-self string-ptr) (p-b const-string-ptr))

(defifun ("string_operator_plus_eq_char" string-operator-plus-eq-char) :void
 (p-self string-ptr) (p-b :uint32))

(defifun ("string_operator_plus_eq_cstr" string-operator-plus-eq-cstr) :void
 (p-self string-ptr) (p-b (:pointer :char)))

(defifun ("string_operator_plus_eq_wcstr" string-operator-plus-eq-wcstr) :void
 (p-self string-ptr) (p-b (:pointer %gdext.util:wchar)))

(defifun ("string_operator_plus_eq_c32str" string-operator-plus-eq-c32str)
 :void (p-self string-ptr) (p-b (:pointer :uint32)))

(defifun ("string_resize" string-resize) int (p-self string-ptr) (p-resize int))

(defifun
 ("string_name_new_with_latin1_chars" string-name-new-with-latin1-chars) :void
 (r-dest uninitialized-string-name-ptr) (p-contents (:pointer :char))
 (p-is-static bool))

(defifun ("string_name_new_with_utf8_chars" string-name-new-with-utf8-chars)
 :void (r-dest uninitialized-string-name-ptr) (p-contents (:pointer :char)))

(defifun
 ("string_name_new_with_utf8_chars_and_len"
  string-name-new-with-utf8-chars-and-len)
 :void (r-dest uninitialized-string-name-ptr) (p-contents (:pointer :char))
 (p-size int))

(defifun ("xml_parser_open_buffer" xml-parser-open-buffer) int
 (p-instance object-ptr) (p-buffer (:pointer :uint8)) (p-size :size))

(defifun ("file_access_store_buffer" file-access-store-buffer) :void
 (p-instance object-ptr) (p-src (:pointer :uint8)) (p-length :uint64))

(defifun ("file_access_get_buffer" file-access-get-buffer) :uint64
 (p-instance const-object-ptr) (p-dst (:pointer :uint8)) (p-length :uint64))

(defifun ("image_ptrw" image-ptrw) (:pointer :uint8) (p-instance object-ptr))

(defifun ("image_ptr" image-ptr) (:pointer :uint8) (p-instance object-ptr))

(defifun
 ("worker_thread_pool_add_native_group_task"
  worker-thread-pool-add-native-group-task)
 :int64 (p-instance object-ptr) (p-func worker-thread-pool-group-task)
 (p-userdata (:pointer :void)) (p-elements :int32) (p-tasks :int32)
 (p-high-priority bool) (p-description const-string-ptr))

(defifun
 ("worker_thread_pool_add_native_task" worker-thread-pool-add-native-task)
 :int64 (p-instance object-ptr) (p-func worker-thread-pool-task)
 (p-userdata (:pointer :void)) (p-high-priority bool)
 (p-description const-string-ptr))

(defifun ("packed_byte_array_operator_index" packed-byte-array-operator-index)
 (:pointer :uint8) (p-self type-ptr) (p-index int))

(defifun
 ("packed_byte_array_operator_index_const"
  packed-byte-array-operator-index-const)
 (:pointer :uint8) (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_float32_array_operator_index" packed-float32-array-operator-index)
 (:pointer :float) (p-self type-ptr) (p-index int))

(defifun
 ("packed_float32_array_operator_index_const"
  packed-float32-array-operator-index-const)
 (:pointer :float) (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_float64_array_operator_index" packed-float64-array-operator-index)
 (:pointer :double) (p-self type-ptr) (p-index int))

(defifun
 ("packed_float64_array_operator_index_const"
  packed-float64-array-operator-index-const)
 (:pointer :double) (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_int32_array_operator_index" packed-int32-array-operator-index)
 (:pointer :int32) (p-self type-ptr) (p-index int))

(defifun
 ("packed_int32_array_operator_index_const"
  packed-int32-array-operator-index-const)
 (:pointer :int32) (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_int64_array_operator_index" packed-int64-array-operator-index)
 (:pointer :int64) (p-self type-ptr) (p-index int))

(defifun
 ("packed_int64_array_operator_index_const"
  packed-int64-array-operator-index-const)
 (:pointer :int64) (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_string_array_operator_index" packed-string-array-operator-index)
 string-ptr (p-self type-ptr) (p-index int))

(defifun
 ("packed_string_array_operator_index_const"
  packed-string-array-operator-index-const)
 string-ptr (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_vector2_array_operator_index" packed-vector2-array-operator-index)
 type-ptr (p-self type-ptr) (p-index int))

(defifun
 ("packed_vector2_array_operator_index_const"
  packed-vector2-array-operator-index-const)
 type-ptr (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_vector3_array_operator_index" packed-vector3-array-operator-index)
 type-ptr (p-self type-ptr) (p-index int))

(defifun
 ("packed_vector3_array_operator_index_const"
  packed-vector3-array-operator-index-const)
 type-ptr (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_vector4_array_operator_index" packed-vector4-array-operator-index)
 type-ptr (p-self type-ptr) (p-index int))

(defifun
 ("packed_vector4_array_operator_index_const"
  packed-vector4-array-operator-index-const)
 type-ptr (p-self const-type-ptr) (p-index int))

(defifun
 ("packed_color_array_operator_index" packed-color-array-operator-index)
 type-ptr (p-self type-ptr) (p-index int))

(defifun
 ("packed_color_array_operator_index_const"
  packed-color-array-operator-index-const)
 type-ptr (p-self const-type-ptr) (p-index int))

(defifun ("array_operator_index" array-operator-index) variant-ptr
 (p-self type-ptr) (p-index int))

(defifun ("array_operator_index_const" array-operator-index-const) variant-ptr
 (p-self const-type-ptr) (p-index int))

(defifun ("array_ref" array-ref) :void (p-self type-ptr)
 (p-from const-type-ptr))

(defifun ("array_set_typed" array-set-typed) :void (p-self type-ptr)
 (p-type variant-type) (p-class-name const-string-name-ptr)
 (p-script const-variant-ptr))

(defifun ("dictionary_operator_index" dictionary-operator-index) variant-ptr
 (p-self type-ptr) (p-key const-variant-ptr))

(defifun ("dictionary_operator_index_const" dictionary-operator-index-const)
 variant-ptr (p-self const-type-ptr) (p-key const-variant-ptr))

(defifun ("dictionary_set_typed" dictionary-set-typed) :void (p-self type-ptr)
 (p-key-type variant-type) (p-key-class-name const-string-name-ptr)
 (p-key-script const-variant-ptr) (p-value-type variant-type)
 (p-value-class-name const-string-name-ptr) (p-value-script const-variant-ptr))

(defifun ("object_method_bind_call" object-method-bind-call) :void
 (p-method-bind method-bind-ptr) (p-instance object-ptr)
 (p-args (:pointer const-variant-ptr)) (p-arg-count int)
 (r-ret uninitialized-variant-ptr) (r-error (:pointer call-error)))

(defifun ("object_method_bind_ptrcall" object-method-bind-ptrcall) :void
 (p-method-bind method-bind-ptr) (p-instance object-ptr)
 (p-args (:pointer const-type-ptr)) (r-ret type-ptr))

(defifun ("object_destroy" object-destroy) :void (p-o object-ptr))

(defifun ("global_get_singleton" global-get-singleton) object-ptr
 (p-name const-string-name-ptr))

(defifun ("object_get_instance_binding" object-get-instance-binding)
 (:pointer :void) (p-o object-ptr) (p-token (:pointer :void))
 (p-callbacks (:pointer instance-binding-callbacks)))

(defifun ("object_set_instance_binding" object-set-instance-binding) :void
 (p-o object-ptr) (p-token (:pointer :void)) (p-binding (:pointer :void))
 (p-callbacks (:pointer instance-binding-callbacks)))

(defifun ("object_free_instance_binding" object-free-instance-binding) :void
 (p-o object-ptr) (p-token (:pointer :void)))

(defifun ("object_set_instance" object-set-instance) :void (p-o object-ptr)
 (p-classname const-string-name-ptr) (p-instance class-instance-ptr))

(defifun ("object_get_class_name" object-get-class-name) bool
 (p-object const-object-ptr) (p-library class-library-ptr)
 (r-class-name uninitialized-string-name-ptr))

(defifun ("object_cast_to" object-cast-to) object-ptr
 (p-object const-object-ptr) (p-class-tag (:pointer :void)))

(defifun ("object_get_instance_from_id" object-get-instance-from-id) object-ptr
 (p-instance-id instance-id))

(defifun ("object_get_instance_id" object-get-instance-id) instance-id
 (p-object const-object-ptr))

(defifun ("object_has_script_method" object-has-script-method) bool
 (p-object const-object-ptr) (p-method const-string-name-ptr))

(defifun ("object_call_script_method" object-call-script-method) :void
 (p-object object-ptr) (p-method const-string-name-ptr)
 (p-args (:pointer const-variant-ptr)) (p-argument-count int)
 (r-return uninitialized-variant-ptr) (r-error (:pointer call-error)))

(defifun ("ref_get_object" ref-get-object) object-ptr (p-ref const-ref-ptr))

(defifun ("ref_set_object" ref-set-object) :void (p-ref ref-ptr)
 (p-object object-ptr))

(defifun ("script_instance_create" script-instance-create) script-instance-ptr
 (p-info (:pointer script-instance-info))
 (p-instance-data script-instance-data-ptr))

(defifun ("script_instance_create2" script-instance-create2)
 script-instance-ptr (p-info (:pointer script-instance-info-2))
 (p-instance-data script-instance-data-ptr))

(defifun ("script_instance_create3" script-instance-create3)
 script-instance-ptr (p-info (:pointer script-instance-info-3))
 (p-instance-data script-instance-data-ptr))

(defifun
 ("placeholder_script_instance_create" placeholder-script-instance-create)
 script-instance-ptr (p-language object-ptr) (p-script object-ptr)
 (p-owner object-ptr))

(defifun
 ("placeholder_script_instance_update" placeholder-script-instance-update)
 :void (p-placeholder script-instance-ptr) (p-properties const-type-ptr)
 (p-values const-type-ptr))

(defifun ("object_get_script_instance" object-get-script-instance)
 script-instance-data-ptr (p-object const-object-ptr) (p-language object-ptr))

(defifun ("object_set_script_instance" object-set-script-instance) :void
 (p-object object-ptr) (p-script-instance script-instance-data-ptr))

(defifun ("callable_custom_create" callable-custom-create) :void
 (r-callable uninitialized-type-ptr)
 (p-callable-custom-info (:pointer callable-custom-info)))

(defifun ("callable_custom_create2" callable-custom-create2) :void
 (r-callable uninitialized-type-ptr)
 (p-callable-custom-info (:pointer callable-custom-info-2)))

(defifun ("callable_custom_get_userdata" callable-custom-get-userdata)
 (:pointer :void) (p-callable const-type-ptr) (p-token (:pointer :void)))

(defifun ("classdb_construct_object" classdb-construct-object) object-ptr
 (p-classname const-string-name-ptr))

(defifun ("classdb_construct_object2" classdb-construct-object2) object-ptr
 (p-classname const-string-name-ptr))

(defifun ("classdb_get_method_bind" classdb-get-method-bind) method-bind-ptr
 (p-classname const-string-name-ptr) (p-methodname const-string-name-ptr)
 (p-hash int))

(defifun ("classdb_get_class_tag" classdb-get-class-tag) (:pointer :void)
 (p-classname const-string-name-ptr))

(defifun ("classdb_register_extension_class" classdb-register-extension-class)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-parent-class-name const-string-name-ptr)
 (p-extension-funcs (:pointer class-creation-info)))

(defifun
 ("classdb_register_extension_class2" classdb-register-extension-class2) :void
 (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-parent-class-name const-string-name-ptr)
 (p-extension-funcs (:pointer class-creation-info-2)))

(defifun
 ("classdb_register_extension_class3" classdb-register-extension-class3) :void
 (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-parent-class-name const-string-name-ptr)
 (p-extension-funcs (:pointer class-creation-info-3)))

(defifun
 ("classdb_register_extension_class4" classdb-register-extension-class4) :void
 (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-parent-class-name const-string-name-ptr)
 (p-extension-funcs (:pointer class-creation-info-4)))

(defifun
 ("classdb_register_extension_class5" classdb-register-extension-class5) :void
 (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-parent-class-name const-string-name-ptr)
 (p-extension-funcs (:pointer class-creation-info-5)))

(defifun
 ("classdb_register_extension_class_method"
  classdb-register-extension-class-method)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-method-info (:pointer class-method-info)))

(defifun
 ("classdb_register_extension_class_virtual_method"
  classdb-register-extension-class-virtual-method)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-method-info (:pointer class-virtual-method-info)))

(defifun
 ("classdb_register_extension_class_integer_constant"
  classdb-register-extension-class-integer-constant)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-enum-name const-string-name-ptr) (p-constant-name const-string-name-ptr)
 (p-constant-value int) (p-is-bitfield bool))

(defifun
 ("classdb_register_extension_class_property"
  classdb-register-extension-class-property)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-info (:pointer property-info)) (p-setter const-string-name-ptr)
 (p-getter const-string-name-ptr))

(defifun
 ("classdb_register_extension_class_property_indexed"
  classdb-register-extension-class-property-indexed)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-info (:pointer property-info)) (p-setter const-string-name-ptr)
 (p-getter const-string-name-ptr) (p-index int))

(defifun
 ("classdb_register_extension_class_property_group"
  classdb-register-extension-class-property-group)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-group-name const-string-ptr) (p-prefix const-string-ptr))

(defifun
 ("classdb_register_extension_class_property_subgroup"
  classdb-register-extension-class-property-subgroup)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-subgroup-name const-string-ptr) (p-prefix const-string-ptr))

(defifun
 ("classdb_register_extension_class_signal"
  classdb-register-extension-class-signal)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr)
 (p-signal-name const-string-name-ptr)
 (p-argument-info (:pointer property-info)) (p-argument-count int))

(defifun
 ("classdb_unregister_extension_class" classdb-unregister-extension-class)
 :void (p-library class-library-ptr) (p-class-name const-string-name-ptr))

(defifun ("get_library_path" get-library-path) :void
 (p-library class-library-ptr) (r-path uninitialized-string-ptr))

(defifun ("editor_add_plugin" editor-add-plugin) :void
 (p-class-name const-string-name-ptr))

(defifun ("editor_remove_plugin" editor-remove-plugin) :void
 (p-class-name const-string-name-ptr))

(defifun
 ("editor_help_load_xml_from_utf8_chars" editor-help-load-xml-from-utf8-chars)
 :void (p-data (:pointer :char)))

(defifun
 ("editor_help_load_xml_from_utf8_chars_and_len"
  editor-help-load-xml-from-utf8-chars-and-len)
 :void (p-data (:pointer :char)) (p-size int))

(defifun
 ("editor_register_get_classes_used_callback"
  editor-register-get-classes-used-callback)
 :void (p-library class-library-ptr)
 (p-callback editor-get-classes-used-callback))

(defifun ("register_main_loop_callbacks" register-main-loop-callbacks) :void
 (p-library class-library-ptr) (p-callbacks (:pointer main-loop-callbacks)))