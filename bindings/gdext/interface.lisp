(common-lisp:in-package :%gdext)


(defifun ("get_godot_version" get-godot-version) :void
 (r-godot-version (:pointer %gdext:godot-version)))

(defifun ("get_godot_version2" get-godot-version2) :void
 (r-godot-version (:pointer %gdext:godot-version-2)))

(defifun ("mem_alloc" mem-alloc) (:pointer :void) (p-bytes :size))

(defifun ("mem_realloc" mem-realloc) (:pointer :void) (p-ptr (:pointer :void))
 (p-bytes :size))

(defifun ("mem_free" mem-free) :void (p-ptr (:pointer :void)))

(defifun ("mem_alloc2" mem-alloc2) (:pointer :void) (p-bytes :size)
 (p-pad-align %gdext:bool))

(defifun ("mem_realloc2" mem-realloc2) (:pointer :void)
 (p-ptr (:pointer :void)) (p-bytes :size) (p-pad-align %gdext:bool))

(defifun ("mem_free2" mem-free2) :void (p-ptr (:pointer :void))
 (p-pad-align %gdext:bool))

(defifun ("print_error" print-error) :void (p-description (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify %gdext:bool))

(defifun ("print_error_with_message" print-error-with-message) :void
 (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify %gdext:bool))

(defifun ("print_warning" print-warning) :void (p-description (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify %gdext:bool))

(defifun ("print_warning_with_message" print-warning-with-message) :void
 (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify %gdext:bool))

(defifun ("print_script_error" print-script-error) :void
 (p-description (:pointer :char)) (p-function (:pointer :char))
 (p-file (:pointer :char)) (p-line :int32) (p-editor-notify %gdext:bool))

(defifun ("print_script_error_with_message" print-script-error-with-message)
 :void (p-description (:pointer :char)) (p-message (:pointer :char))
 (p-function (:pointer :char)) (p-file (:pointer :char)) (p-line :int32)
 (p-editor-notify %gdext:bool))

(defifun ("get_native_struct_size" get-native-struct-size) :uint64
 (p-name %gdext:const-string-name-ptr))

(defifun ("variant_new_copy" variant-new-copy) :void
 (r-dest %gdext:uninitialized-variant-ptr) (p-src %gdext:const-variant-ptr))

(defifun ("variant_new_nil" variant-new-nil) :void
 (r-dest %gdext:uninitialized-variant-ptr))

(defifun ("variant_destroy" variant-destroy) :void (p-self %gdext:variant-ptr))

(defifun ("variant_call" variant-call) :void (p-self %gdext:variant-ptr)
 (p-method %gdext:const-string-name-ptr)
 (p-args (:pointer %gdext:const-variant-ptr)) (p-argument-count %gdext:int)
 (r-return %gdext:uninitialized-variant-ptr)
 (r-error (:pointer %gdext:call-error)))

(defifun ("variant_call_static" variant-call-static) :void
 (p-type %gdext:variant-type) (p-method %gdext:const-string-name-ptr)
 (p-args (:pointer %gdext:const-variant-ptr)) (p-argument-count %gdext:int)
 (r-return %gdext:uninitialized-variant-ptr)
 (r-error (:pointer %gdext:call-error)))

(defifun ("variant_evaluate" variant-evaluate) :void
 (p-op %gdext:variant-operator) (p-a %gdext:const-variant-ptr)
 (p-b %gdext:const-variant-ptr) (r-return %gdext:uninitialized-variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_set" variant-set) :void (p-self %gdext:variant-ptr)
 (p-key %gdext:const-variant-ptr) (p-value %gdext:const-variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_set_named" variant-set-named) :void
 (p-self %gdext:variant-ptr) (p-key %gdext:const-string-name-ptr)
 (p-value %gdext:const-variant-ptr) (r-valid (:pointer %gdext:bool)))

(defifun ("variant_set_keyed" variant-set-keyed) :void
 (p-self %gdext:variant-ptr) (p-key %gdext:const-variant-ptr)
 (p-value %gdext:const-variant-ptr) (r-valid (:pointer %gdext:bool)))

(defifun ("variant_set_indexed" variant-set-indexed) :void
 (p-self %gdext:variant-ptr) (p-index %gdext:int)
 (p-value %gdext:const-variant-ptr) (r-valid (:pointer %gdext:bool))
 (r-oob (:pointer %gdext:bool)))

(defifun ("variant_get" variant-get) :void (p-self %gdext:const-variant-ptr)
 (p-key %gdext:const-variant-ptr) (r-ret %gdext:uninitialized-variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_get_named" variant-get-named) :void
 (p-self %gdext:const-variant-ptr) (p-key %gdext:const-string-name-ptr)
 (r-ret %gdext:uninitialized-variant-ptr) (r-valid (:pointer %gdext:bool)))

(defifun ("variant_get_keyed" variant-get-keyed) :void
 (p-self %gdext:const-variant-ptr) (p-key %gdext:const-variant-ptr)
 (r-ret %gdext:uninitialized-variant-ptr) (r-valid (:pointer %gdext:bool)))

(defifun ("variant_get_indexed" variant-get-indexed) :void
 (p-self %gdext:const-variant-ptr) (p-index %gdext:int)
 (r-ret %gdext:uninitialized-variant-ptr) (r-valid (:pointer %gdext:bool))
 (r-oob (:pointer %gdext:bool)))

(defifun ("variant_iter_init" variant-iter-init) %gdext:bool
 (p-self %gdext:const-variant-ptr) (r-iter %gdext:uninitialized-variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_iter_next" variant-iter-next) %gdext:bool
 (p-self %gdext:const-variant-ptr) (r-iter %gdext:variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_iter_get" variant-iter-get) :void
 (p-self %gdext:const-variant-ptr) (r-iter %gdext:variant-ptr)
 (r-ret %gdext:uninitialized-variant-ptr) (r-valid (:pointer %gdext:bool)))

(defifun ("variant_hash" variant-hash) %gdext:int
 (p-self %gdext:const-variant-ptr))

(defifun ("variant_recursive_hash" variant-recursive-hash) %gdext:int
 (p-self %gdext:const-variant-ptr) (p-recursion-count %gdext:int))

(defifun ("variant_hash_compare" variant-hash-compare) %gdext:bool
 (p-self %gdext:const-variant-ptr) (p-other %gdext:const-variant-ptr))

(defifun ("variant_booleanize" variant-booleanize) %gdext:bool
 (p-self %gdext:const-variant-ptr))

(defifun ("variant_duplicate" variant-duplicate) :void
 (p-self %gdext:const-variant-ptr) (r-ret %gdext:variant-ptr)
 (p-deep %gdext:bool))

(defifun ("variant_stringify" variant-stringify) :void
 (p-self %gdext:const-variant-ptr) (r-ret %gdext:string-ptr))

(defifun ("variant_get_type" variant-get-type) %gdext:variant-type
 (p-self %gdext:const-variant-ptr))

(defifun ("variant_has_method" variant-has-method) %gdext:bool
 (p-self %gdext:const-variant-ptr) (p-method %gdext:const-string-name-ptr))

(defifun ("variant_has_member" variant-has-member) %gdext:bool
 (p-type %gdext:variant-type) (p-member %gdext:const-string-name-ptr))

(defifun ("variant_has_key" variant-has-key) %gdext:bool
 (p-self %gdext:const-variant-ptr) (p-key %gdext:const-variant-ptr)
 (r-valid (:pointer %gdext:bool)))

(defifun ("variant_get_object_instance_id" variant-get-object-instance-id)
 %gdext:instance-id (p-self %gdext:const-variant-ptr))

(defifun ("variant_get_type_name" variant-get-type-name) :void
 (p-type %gdext:variant-type) (r-name %gdext:uninitialized-string-ptr))

(defifun ("variant_can_convert" variant-can-convert) %gdext:bool
 (p-from %gdext:variant-type) (p-to %gdext:variant-type))

(defifun ("variant_can_convert_strict" variant-can-convert-strict) %gdext:bool
 (p-from %gdext:variant-type) (p-to %gdext:variant-type))

(defifun
 ("get_variant_from_type_constructor" get-variant-from-type-constructor)
 %gdext:variant-from-type-constructor-func (p-type %gdext:variant-type))

(defifun ("get_variant_to_type_constructor" get-variant-to-type-constructor)
 %gdext:type-from-variant-constructor-func (p-type %gdext:variant-type))

(defifun ("variant_get_ptr_internal_getter" variant-get-ptr-internal-getter)
 %gdext:variant-get-internal-ptr-func (p-type %gdext:variant-type))

(defifun
 ("variant_get_ptr_operator_evaluator" variant-get-ptr-operator-evaluator)
 %gdext:ptr-operator-evaluator (p-operator %gdext:variant-operator)
 (p-type-a %gdext:variant-type) (p-type-b %gdext:variant-type))

(defifun ("variant_get_ptr_builtin_method" variant-get-ptr-builtin-method)
 %gdext:ptr-built-in-method (p-type %gdext:variant-type)
 (p-method %gdext:const-string-name-ptr) (p-hash %gdext:int))

(defifun ("variant_get_ptr_constructor" variant-get-ptr-constructor)
 %gdext:ptr-constructor (p-type %gdext:variant-type) (p-constructor :int32))

(defifun ("variant_get_ptr_destructor" variant-get-ptr-destructor)
 %gdext:ptr-destructor (p-type %gdext:variant-type))

(defifun ("variant_construct" variant-construct) :void
 (p-type %gdext:variant-type) (r-base %gdext:uninitialized-variant-ptr)
 (p-args (:pointer %gdext:const-variant-ptr)) (p-argument-count :int32)
 (r-error (:pointer %gdext:call-error)))

(defifun ("variant_get_ptr_setter" variant-get-ptr-setter) %gdext:ptr-setter
 (p-type %gdext:variant-type) (p-member %gdext:const-string-name-ptr))

(defifun ("variant_get_ptr_getter" variant-get-ptr-getter) %gdext:ptr-getter
 (p-type %gdext:variant-type) (p-member %gdext:const-string-name-ptr))

(defifun ("variant_get_ptr_indexed_setter" variant-get-ptr-indexed-setter)
 %gdext:ptr-indexed-setter (p-type %gdext:variant-type))

(defifun ("variant_get_ptr_indexed_getter" variant-get-ptr-indexed-getter)
 %gdext:ptr-indexed-getter (p-type %gdext:variant-type))

(defifun ("variant_get_ptr_keyed_setter" variant-get-ptr-keyed-setter)
 %gdext:ptr-keyed-setter (p-type %gdext:variant-type))

(defifun ("variant_get_ptr_keyed_getter" variant-get-ptr-keyed-getter)
 %gdext:ptr-keyed-getter (p-type %gdext:variant-type))

(defifun ("variant_get_ptr_keyed_checker" variant-get-ptr-keyed-checker)
 %gdext:ptr-keyed-checker (p-type %gdext:variant-type))

(defifun ("variant_get_constant_value" variant-get-constant-value) :void
 (p-type %gdext:variant-type) (p-constant %gdext:const-string-name-ptr)
 (r-ret %gdext:uninitialized-variant-ptr))

(defifun ("variant_get_ptr_utility_function" variant-get-ptr-utility-function)
 %gdext:ptr-utility-function (p-function %gdext:const-string-name-ptr)
 (p-hash %gdext:int))

(defifun ("string_new_with_latin1_chars" string-new-with-latin1-chars) :void
 (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :char)))

(defifun ("string_new_with_utf8_chars" string-new-with-utf8-chars) :void
 (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :char)))

(defifun ("string_new_with_utf16_chars" string-new-with-utf16-chars) :void
 (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :uint16)))

(defifun ("string_new_with_utf32_chars" string-new-with-utf32-chars) :void
 (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :uint32)))

(defifun ("string_new_with_wide_chars" string-new-with-wide-chars) :void
 (r-dest %gdext:uninitialized-string-ptr)
 (p-contents (:pointer %gdext.util:wchar)))

(defifun
 ("string_new_with_latin1_chars_and_len" string-new-with-latin1-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :char))
 (p-size %gdext:int))

(defifun
 ("string_new_with_utf8_chars_and_len" string-new-with-utf8-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :char))
 (p-size %gdext:int))

(defifun
 ("string_new_with_utf8_chars_and_len2" string-new-with-utf8-chars-and-len2)
 %gdext:int (r-dest %gdext:uninitialized-string-ptr)
 (p-contents (:pointer :char)) (p-size %gdext:int))

(defifun
 ("string_new_with_utf16_chars_and_len" string-new-with-utf16-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :uint16))
 (p-char-count %gdext:int))

(defifun
 ("string_new_with_utf16_chars_and_len2" string-new-with-utf16-chars-and-len2)
 %gdext:int (r-dest %gdext:uninitialized-string-ptr)
 (p-contents (:pointer :uint16)) (p-char-count %gdext:int)
 (p-default-little-endian %gdext:bool))

(defifun
 ("string_new_with_utf32_chars_and_len" string-new-with-utf32-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-ptr) (p-contents (:pointer :uint32))
 (p-char-count %gdext:int))

(defifun
 ("string_new_with_wide_chars_and_len" string-new-with-wide-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-ptr)
 (p-contents (:pointer %gdext.util:wchar)) (p-char-count %gdext:int))

(defifun ("string_to_latin1_chars" string-to-latin1-chars) %gdext:int
 (p-self %gdext:const-string-ptr) (r-text (:pointer :char))
 (p-max-write-length %gdext:int))

(defifun ("string_to_utf8_chars" string-to-utf8-chars) %gdext:int
 (p-self %gdext:const-string-ptr) (r-text (:pointer :char))
 (p-max-write-length %gdext:int))

(defifun ("string_to_utf16_chars" string-to-utf16-chars) %gdext:int
 (p-self %gdext:const-string-ptr) (r-text (:pointer :uint16))
 (p-max-write-length %gdext:int))

(defifun ("string_to_utf32_chars" string-to-utf32-chars) %gdext:int
 (p-self %gdext:const-string-ptr) (r-text (:pointer :uint32))
 (p-max-write-length %gdext:int))

(defifun ("string_to_wide_chars" string-to-wide-chars) %gdext:int
 (p-self %gdext:const-string-ptr) (r-text (:pointer %gdext.util:wchar))
 (p-max-write-length %gdext:int))

(defifun ("string_operator_index" string-operator-index) (:pointer :uint32)
 (p-self %gdext:string-ptr) (p-index %gdext:int))

(defifun ("string_operator_index_const" string-operator-index-const)
 (:pointer :uint32) (p-self %gdext:const-string-ptr) (p-index %gdext:int))

(defifun ("string_operator_plus_eq_string" string-operator-plus-eq-string)
 :void (p-self %gdext:string-ptr) (p-b %gdext:const-string-ptr))

(defifun ("string_operator_plus_eq_char" string-operator-plus-eq-char) :void
 (p-self %gdext:string-ptr) (p-b :uint32))

(defifun ("string_operator_plus_eq_cstr" string-operator-plus-eq-cstr) :void
 (p-self %gdext:string-ptr) (p-b (:pointer :char)))

(defifun ("string_operator_plus_eq_wcstr" string-operator-plus-eq-wcstr) :void
 (p-self %gdext:string-ptr) (p-b (:pointer %gdext.util:wchar)))

(defifun ("string_operator_plus_eq_c32str" string-operator-plus-eq-c32str)
 :void (p-self %gdext:string-ptr) (p-b (:pointer :uint32)))

(defifun ("string_resize" string-resize) %gdext:int (p-self %gdext:string-ptr)
 (p-resize %gdext:int))

(defifun
 ("string_name_new_with_latin1_chars" string-name-new-with-latin1-chars) :void
 (r-dest %gdext:uninitialized-string-name-ptr) (p-contents (:pointer :char))
 (p-is-static %gdext:bool))

(defifun ("string_name_new_with_utf8_chars" string-name-new-with-utf8-chars)
 :void (r-dest %gdext:uninitialized-string-name-ptr)
 (p-contents (:pointer :char)))

(defifun
 ("string_name_new_with_utf8_chars_and_len"
  string-name-new-with-utf8-chars-and-len)
 :void (r-dest %gdext:uninitialized-string-name-ptr)
 (p-contents (:pointer :char)) (p-size %gdext:int))

(defifun ("xml_parser_open_buffer" xml-parser-open-buffer) %gdext:int
 (p-instance %gdext:object-ptr) (p-buffer (:pointer :uint8)) (p-size :size))

(defifun ("file_access_store_buffer" file-access-store-buffer) :void
 (p-instance %gdext:object-ptr) (p-src (:pointer :uint8)) (p-length :uint64))

(defifun ("file_access_get_buffer" file-access-get-buffer) :uint64
 (p-instance %gdext:const-object-ptr) (p-dst (:pointer :uint8))
 (p-length :uint64))

(defifun ("image_ptrw" image-ptrw) (:pointer :uint8)
 (p-instance %gdext:object-ptr))

(defifun ("image_ptr" image-ptr) (:pointer :uint8)
 (p-instance %gdext:object-ptr))

(defifun
 ("worker_thread_pool_add_native_group_task"
  worker-thread-pool-add-native-group-task)
 :int64 (p-instance %gdext:object-ptr)
 (p-func %gdext:worker-thread-pool-group-task) (p-userdata (:pointer :void))
 (p-elements :int32) (p-tasks :int32) (p-high-priority %gdext:bool)
 (p-description %gdext:const-string-ptr))

(defifun
 ("worker_thread_pool_add_native_task" worker-thread-pool-add-native-task)
 :int64 (p-instance %gdext:object-ptr) (p-func %gdext:worker-thread-pool-task)
 (p-userdata (:pointer :void)) (p-high-priority %gdext:bool)
 (p-description %gdext:const-string-ptr))

(defifun ("packed_byte_array_operator_index" packed-byte-array-operator-index)
 (:pointer :uint8) (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_byte_array_operator_index_const"
  packed-byte-array-operator-index-const)
 (:pointer :uint8) (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_float32_array_operator_index" packed-float32-array-operator-index)
 (:pointer :float) (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_float32_array_operator_index_const"
  packed-float32-array-operator-index-const)
 (:pointer :float) (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_float64_array_operator_index" packed-float64-array-operator-index)
 (:pointer :double) (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_float64_array_operator_index_const"
  packed-float64-array-operator-index-const)
 (:pointer :double) (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_int32_array_operator_index" packed-int32-array-operator-index)
 (:pointer :int32) (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_int32_array_operator_index_const"
  packed-int32-array-operator-index-const)
 (:pointer :int32) (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_int64_array_operator_index" packed-int64-array-operator-index)
 (:pointer :int64) (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_int64_array_operator_index_const"
  packed-int64-array-operator-index-const)
 (:pointer :int64) (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_string_array_operator_index" packed-string-array-operator-index)
 %gdext:string-ptr (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_string_array_operator_index_const"
  packed-string-array-operator-index-const)
 %gdext:string-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector2_array_operator_index" packed-vector2-array-operator-index)
 %gdext:type-ptr (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector2_array_operator_index_const"
  packed-vector2-array-operator-index-const)
 %gdext:type-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector3_array_operator_index" packed-vector3-array-operator-index)
 %gdext:type-ptr (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector3_array_operator_index_const"
  packed-vector3-array-operator-index-const)
 %gdext:type-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector4_array_operator_index" packed-vector4-array-operator-index)
 %gdext:type-ptr (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_vector4_array_operator_index_const"
  packed-vector4-array-operator-index-const)
 %gdext:type-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun
 ("packed_color_array_operator_index" packed-color-array-operator-index)
 %gdext:type-ptr (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun
 ("packed_color_array_operator_index_const"
  packed-color-array-operator-index-const)
 %gdext:type-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun ("array_operator_index" array-operator-index) %gdext:variant-ptr
 (p-self %gdext:type-ptr) (p-index %gdext:int))

(defifun ("array_operator_index_const" array-operator-index-const)
 %gdext:variant-ptr (p-self %gdext:const-type-ptr) (p-index %gdext:int))

(defifun ("array_ref" array-ref) :void (p-self %gdext:type-ptr)
 (p-from %gdext:const-type-ptr))

(defifun ("array_set_typed" array-set-typed) :void (p-self %gdext:type-ptr)
 (p-type %gdext:variant-type) (p-class-name %gdext:const-string-name-ptr)
 (p-script %gdext:const-variant-ptr))

(defifun ("dictionary_operator_index" dictionary-operator-index)
 %gdext:variant-ptr (p-self %gdext:type-ptr) (p-key %gdext:const-variant-ptr))

(defifun ("dictionary_operator_index_const" dictionary-operator-index-const)
 %gdext:variant-ptr (p-self %gdext:const-type-ptr)
 (p-key %gdext:const-variant-ptr))

(defifun ("dictionary_set_typed" dictionary-set-typed) :void
 (p-self %gdext:type-ptr) (p-key-type %gdext:variant-type)
 (p-key-class-name %gdext:const-string-name-ptr)
 (p-key-script %gdext:const-variant-ptr) (p-value-type %gdext:variant-type)
 (p-value-class-name %gdext:const-string-name-ptr)
 (p-value-script %gdext:const-variant-ptr))

(defifun ("object_method_bind_call" object-method-bind-call) :void
 (p-method-bind %gdext:method-bind-ptr) (p-instance %gdext:object-ptr)
 (p-args (:pointer %gdext:const-variant-ptr)) (p-arg-count %gdext:int)
 (r-ret %gdext:uninitialized-variant-ptr)
 (r-error (:pointer %gdext:call-error)))

(defifun ("object_method_bind_ptrcall" object-method-bind-ptrcall) :void
 (p-method-bind %gdext:method-bind-ptr) (p-instance %gdext:object-ptr)
 (p-args (:pointer %gdext:const-type-ptr)) (r-ret %gdext:type-ptr))

(defifun ("object_destroy" object-destroy) :void (p-o %gdext:object-ptr))

(defifun ("global_get_singleton" global-get-singleton) %gdext:object-ptr
 (p-name %gdext:const-string-name-ptr))

(defifun ("object_get_instance_binding" object-get-instance-binding)
 (:pointer :void) (p-o %gdext:object-ptr) (p-token (:pointer :void))
 (p-callbacks (:pointer %gdext:instance-binding-callbacks)))

(defifun ("object_set_instance_binding" object-set-instance-binding) :void
 (p-o %gdext:object-ptr) (p-token (:pointer :void))
 (p-binding (:pointer :void))
 (p-callbacks (:pointer %gdext:instance-binding-callbacks)))

(defifun ("object_free_instance_binding" object-free-instance-binding) :void
 (p-o %gdext:object-ptr) (p-token (:pointer :void)))

(defifun ("object_set_instance" object-set-instance) :void
 (p-o %gdext:object-ptr) (p-classname %gdext:const-string-name-ptr)
 (p-instance %gdext:class-instance-ptr))

(defifun ("object_get_class_name" object-get-class-name) %gdext:bool
 (p-object %gdext:const-object-ptr) (p-library %gdext:class-library-ptr)
 (r-class-name %gdext:uninitialized-string-name-ptr))

(defifun ("object_cast_to" object-cast-to) %gdext:object-ptr
 (p-object %gdext:const-object-ptr) (p-class-tag (:pointer :void)))

(defifun ("object_get_instance_from_id" object-get-instance-from-id)
 %gdext:object-ptr (p-instance-id %gdext:instance-id))

(defifun ("object_get_instance_id" object-get-instance-id) %gdext:instance-id
 (p-object %gdext:const-object-ptr))

(defifun ("object_has_script_method" object-has-script-method) %gdext:bool
 (p-object %gdext:const-object-ptr) (p-method %gdext:const-string-name-ptr))

(defifun ("object_call_script_method" object-call-script-method) :void
 (p-object %gdext:object-ptr) (p-method %gdext:const-string-name-ptr)
 (p-args (:pointer %gdext:const-variant-ptr)) (p-argument-count %gdext:int)
 (r-return %gdext:uninitialized-variant-ptr)
 (r-error (:pointer %gdext:call-error)))

(defifun ("ref_get_object" ref-get-object) %gdext:object-ptr
 (p-ref %gdext:const-ref-ptr))

(defifun ("ref_set_object" ref-set-object) :void (p-ref %gdext:ref-ptr)
 (p-object %gdext:object-ptr))

(defifun ("script_instance_create" script-instance-create)
 %gdext:script-instance-ptr (p-info (:pointer %gdext:script-instance-info))
 (p-instance-data %gdext:script-instance-data-ptr))

(defifun ("script_instance_create2" script-instance-create2)
 %gdext:script-instance-ptr (p-info (:pointer %gdext:script-instance-info-2))
 (p-instance-data %gdext:script-instance-data-ptr))

(defifun ("script_instance_create3" script-instance-create3)
 %gdext:script-instance-ptr (p-info (:pointer %gdext:script-instance-info-3))
 (p-instance-data %gdext:script-instance-data-ptr))

(defifun
 ("placeholder_script_instance_create" placeholder-script-instance-create)
 %gdext:script-instance-ptr (p-language %gdext:object-ptr)
 (p-script %gdext:object-ptr) (p-owner %gdext:object-ptr))

(defifun
 ("placeholder_script_instance_update" placeholder-script-instance-update)
 :void (p-placeholder %gdext:script-instance-ptr)
 (p-properties %gdext:const-type-ptr) (p-values %gdext:const-type-ptr))

(defifun ("object_get_script_instance" object-get-script-instance)
 %gdext:script-instance-data-ptr (p-object %gdext:const-object-ptr)
 (p-language %gdext:object-ptr))

(defifun ("object_set_script_instance" object-set-script-instance) :void
 (p-object %gdext:object-ptr)
 (p-script-instance %gdext:script-instance-data-ptr))

(defifun ("callable_custom_create" callable-custom-create) :void
 (r-callable %gdext:uninitialized-type-ptr)
 (p-callable-custom-info (:pointer %gdext:callable-custom-info)))

(defifun ("callable_custom_create2" callable-custom-create2) :void
 (r-callable %gdext:uninitialized-type-ptr)
 (p-callable-custom-info (:pointer %gdext:callable-custom-info-2)))

(defifun ("callable_custom_get_userdata" callable-custom-get-userdata)
 (:pointer :void) (p-callable %gdext:const-type-ptr) (p-token (:pointer :void)))

(defifun ("classdb_construct_object" classdb-construct-object)
 %gdext:object-ptr (p-classname %gdext:const-string-name-ptr))

(defifun ("classdb_construct_object2" classdb-construct-object2)
 %gdext:object-ptr (p-classname %gdext:const-string-name-ptr))

(defifun ("classdb_get_method_bind" classdb-get-method-bind)
 %gdext:method-bind-ptr (p-classname %gdext:const-string-name-ptr)
 (p-methodname %gdext:const-string-name-ptr) (p-hash %gdext:int))

(defifun ("classdb_get_class_tag" classdb-get-class-tag) (:pointer :void)
 (p-classname %gdext:const-string-name-ptr))

(defifun ("classdb_register_extension_class" classdb-register-extension-class)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-parent-class-name %gdext:const-string-name-ptr)
 (p-extension-funcs (:pointer %gdext:class-creation-info)))

(defifun
 ("classdb_register_extension_class2" classdb-register-extension-class2) :void
 (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-parent-class-name %gdext:const-string-name-ptr)
 (p-extension-funcs (:pointer %gdext:class-creation-info-2)))

(defifun
 ("classdb_register_extension_class3" classdb-register-extension-class3) :void
 (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-parent-class-name %gdext:const-string-name-ptr)
 (p-extension-funcs (:pointer %gdext:class-creation-info-3)))

(defifun
 ("classdb_register_extension_class4" classdb-register-extension-class4) :void
 (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-parent-class-name %gdext:const-string-name-ptr)
 (p-extension-funcs (:pointer %gdext:class-creation-info-4)))

(defifun
 ("classdb_register_extension_class5" classdb-register-extension-class5) :void
 (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-parent-class-name %gdext:const-string-name-ptr)
 (p-extension-funcs (:pointer %gdext:class-creation-info-5)))

(defifun
 ("classdb_register_extension_class_method"
  classdb-register-extension-class-method)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-method-info (:pointer %gdext:class-method-info)))

(defifun
 ("classdb_register_extension_class_virtual_method"
  classdb-register-extension-class-virtual-method)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-method-info (:pointer %gdext:class-virtual-method-info)))

(defifun
 ("classdb_register_extension_class_integer_constant"
  classdb-register-extension-class-integer-constant)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-enum-name %gdext:const-string-name-ptr)
 (p-constant-name %gdext:const-string-name-ptr) (p-constant-value %gdext:int)
 (p-is-bitfield %gdext:bool))

(defifun
 ("classdb_register_extension_class_property"
  classdb-register-extension-class-property)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-info (:pointer %gdext:property-info))
 (p-setter %gdext:const-string-name-ptr)
 (p-getter %gdext:const-string-name-ptr))

(defifun
 ("classdb_register_extension_class_property_indexed"
  classdb-register-extension-class-property-indexed)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-info (:pointer %gdext:property-info))
 (p-setter %gdext:const-string-name-ptr)
 (p-getter %gdext:const-string-name-ptr) (p-index %gdext:int))

(defifun
 ("classdb_register_extension_class_property_group"
  classdb-register-extension-class-property-group)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-group-name %gdext:const-string-ptr) (p-prefix %gdext:const-string-ptr))

(defifun
 ("classdb_register_extension_class_property_subgroup"
  classdb-register-extension-class-property-subgroup)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-subgroup-name %gdext:const-string-ptr) (p-prefix %gdext:const-string-ptr))

(defifun
 ("classdb_register_extension_class_signal"
  classdb-register-extension-class-signal)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr)
 (p-signal-name %gdext:const-string-name-ptr)
 (p-argument-info (:pointer %gdext:property-info))
 (p-argument-count %gdext:int))

(defifun
 ("classdb_unregister_extension_class" classdb-unregister-extension-class)
 :void (p-library %gdext:class-library-ptr)
 (p-class-name %gdext:const-string-name-ptr))

(defifun ("get_library_path" get-library-path) :void
 (p-library %gdext:class-library-ptr) (r-path %gdext:uninitialized-string-ptr))

(defifun ("editor_add_plugin" editor-add-plugin) :void
 (p-class-name %gdext:const-string-name-ptr))

(defifun ("editor_remove_plugin" editor-remove-plugin) :void
 (p-class-name %gdext:const-string-name-ptr))

(defifun
 ("editor_help_load_xml_from_utf8_chars" editor-help-load-xml-from-utf8-chars)
 :void (p-data (:pointer :char)))

(defifun
 ("editor_help_load_xml_from_utf8_chars_and_len"
  editor-help-load-xml-from-utf8-chars-and-len)
 :void (p-data (:pointer :char)) (p-size %gdext:int))

(defifun
 ("editor_register_get_classes_used_callback"
  editor-register-get-classes-used-callback)
 :void (p-library %gdext:class-library-ptr)
 (p-callback %gdext:editor-get-classes-used-callback))

(defifun ("register_main_loop_callbacks" register-main-loop-callbacks) :void
 (p-library %gdext:class-library-ptr)
 (p-callbacks (:pointer %gdext:main-loop-callbacks)))