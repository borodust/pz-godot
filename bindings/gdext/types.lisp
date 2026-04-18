(common-lisp:in-package :%gdext)


(cffi:defcenum variant-type
  (:nil 0)
  (:bool 1)
  (:int 2)
  (:float 3)
  (:string 4)
  (:vector2 5)
  (:vector2i 6)
  (:rect2 7)
  (:rect2i 8)
  (:vector3 9)
  (:vector3i 10)
  (:transform2d 11)
  (:vector4 12)
  (:vector4i 13)
  (:plane 14)
  (:quaternion 15)
  (:aabb 16)
  (:basis 17)
  (:transform3d 18)
  (:projection 19)
  (:color 20)
  (:string-name 21)
  (:node-path 22)
  (:rid 23)
  (:object 24)
  (:callable 25)
  (:signal 26)
  (:dictionary 27)
  (:array 28)
  (:packed-byte-array 29)
  (:packed-int32-array 30)
  (:packed-int64-array 31)
  (:packed-float32-array 32)
  (:packed-float64-array 33)
  (:packed-string-array 34)
  (:packed-vector2-array 35)
  (:packed-vector3-array 36)
  (:packed-color-array 37)
  (:packed-vector4-array 38)
  (:variant-max 39))

(cffi:defcenum variant-operator
  (:equal 0)
  (:not-equal 1)
  (:less 2)
  (:less-equal 3)
  (:greater 4)
  (:greater-equal 5)
  (:add 6)
  (:subtract 7)
  (:multiply 8)
  (:divide 9)
  (:negate 10)
  (:positive 11)
  (:module 12)
  (:power 13)
  (:shift-left 14)
  (:shift-right 15)
  (:bit-and 16)
  (:bit-or 17)
  (:bit-xor 18)
  (:bit-negate 19)
  (:and 20)
  (:or 21)
  (:xor 22)
  (:not 23)
  (:in 24)
  (:max 25))

(cffi:defctype variant-ptr (:pointer :void)
               "In this API there are multiple functions which expect the caller to pass a pointer
on return value as parameter.
In order to make it clear if the caller should initialize the return value or not
we have two flavor of types:
- `GDExtensionXXXPtr` for pointer on an initialized value
- `GDExtensionUninitializedXXXPtr` for pointer on uninitialized value
Notes:
- Not respecting those requirements can seems harmless, but will lead to unexpected
segfault or memory leak (for instance with a specific compiler/OS, or when two
native extensions start doing ptrcall on each other).
- Initialization must be done with the function pointer returned by `variant_get_ptr_constructor`,
zero-initializing the variable should not be considered a valid initialization method here !
- Some types have no destructor (see `extension_api.json`'s `has_destructor` field), for
them it is always safe to skip the constructor for the return value if you are in a hurry ;-)")

(cffi:defctype const-variant-ptr (:pointer :void))

(cffi:defctype uninitialized-variant-ptr (:pointer :void))

(cffi:defctype string-name-ptr (:pointer :void))

(cffi:defctype const-string-name-ptr (:pointer :void))

(cffi:defctype uninitialized-string-name-ptr (:pointer :void))

(cffi:defctype string-ptr (:pointer :void))

(cffi:defctype const-string-ptr (:pointer :void))

(cffi:defctype uninitialized-string-ptr (:pointer :void))

(cffi:defctype object-ptr (:pointer :void))

(cffi:defctype const-object-ptr (:pointer :void))

(cffi:defctype uninitialized-object-ptr (:pointer :void))

(cffi:defctype type-ptr (:pointer :void))

(cffi:defctype const-type-ptr (:pointer :void))

(cffi:defctype uninitialized-type-ptr (:pointer :void))

(cffi:defctype method-bind-ptr (:pointer :void))

(cffi:defctype int :int64)

(cffi:defctype bool :uint8)

(cffi:defctype instance-id :uint64)

(cffi:defctype ref-ptr (:pointer :void))

(cffi:defctype const-ref-ptr (:pointer :void))

(cffi:defcenum call-error-type
  (:ok 0)
  (:error-invalid-method 1)
  (:error-invalid-argument 2)
  (:error-too-many-arguments 3)
  (:error-too-few-arguments 4)
  (:error-instance-is-null 5)
  (:error-method-not-const 6))

(cffi:defcstruct call-error
  (error call-error-type)
  (argument :int32)
  (expected :int32))
(cffi:defctype call-error (:struct call-error))

(cffi:defctype variant-from-type-constructor-func (:pointer :void))

(defcfunproto variant-from-type-constructor-func :void
 (arg0 uninitialized-variant-ptr) (arg1 type-ptr))

(cffi:defctype type-from-variant-constructor-func (:pointer :void))

(defcfunproto type-from-variant-constructor-func :void
 (arg0 uninitialized-type-ptr) (arg1 variant-ptr))

(cffi:defctype variant-get-internal-ptr-func (:pointer :void))

(defcfunproto variant-get-internal-ptr-func (:pointer :void) (arg0 variant-ptr))

(cffi:defctype ptr-operator-evaluator (:pointer :void))

(defcfunproto ptr-operator-evaluator :void (p-left const-type-ptr)
 (p-right const-type-ptr) (r-result type-ptr))

(cffi:defctype ptr-built-in-method (:pointer :void))

(defcfunproto ptr-built-in-method :void (p-base type-ptr)
 (p-args (:pointer const-type-ptr)) (r-return type-ptr)
 (p-argument-count :int32))

(cffi:defctype ptr-constructor (:pointer :void))

(defcfunproto ptr-constructor :void (p-base uninitialized-type-ptr)
 (p-args (:pointer const-type-ptr)))

(cffi:defctype ptr-destructor (:pointer :void))

(defcfunproto ptr-destructor :void (p-base type-ptr))

(cffi:defctype ptr-setter (:pointer :void))

(defcfunproto ptr-setter :void (p-base type-ptr) (p-value const-type-ptr))

(cffi:defctype ptr-getter (:pointer :void))

(defcfunproto ptr-getter :void (p-base const-type-ptr) (r-value type-ptr))

(cffi:defctype ptr-indexed-setter (:pointer :void))

(defcfunproto ptr-indexed-setter :void (p-base type-ptr) (p-index int)
 (p-value const-type-ptr))

(cffi:defctype ptr-indexed-getter (:pointer :void))

(defcfunproto ptr-indexed-getter :void (p-base const-type-ptr) (p-index int)
 (r-value type-ptr))

(cffi:defctype ptr-keyed-setter (:pointer :void))

(defcfunproto ptr-keyed-setter :void (p-base type-ptr) (p-key const-type-ptr)
 (p-value const-type-ptr))

(cffi:defctype ptr-keyed-getter (:pointer :void))

(defcfunproto ptr-keyed-getter :void (p-base const-type-ptr)
 (p-key const-type-ptr) (r-value type-ptr))

(cffi:defctype ptr-keyed-checker (:pointer :void))

(defcfunproto ptr-keyed-checker :uint32 (p-base const-variant-ptr)
 (p-key const-variant-ptr))

(cffi:defctype ptr-utility-function (:pointer :void))

(defcfunproto ptr-utility-function :void (r-return type-ptr)
 (p-args (:pointer const-type-ptr)) (p-argument-count :int32))

(cffi:defctype class-constructor (:pointer :void))

(defcfunproto class-constructor object-ptr)

(cffi:defctype instance-binding-create-callback (:pointer :void))

(defcfunproto instance-binding-create-callback (:pointer :void)
 (p-token (:pointer :void)) (p-instance (:pointer :void)))

(cffi:defctype instance-binding-free-callback (:pointer :void))

(defcfunproto instance-binding-free-callback :void (p-token (:pointer :void))
 (p-instance (:pointer :void)) (p-binding (:pointer :void)))

(cffi:defctype instance-binding-reference-callback (:pointer :void))

(defcfunproto instance-binding-reference-callback bool
 (p-token (:pointer :void)) (p-binding (:pointer :void)) (p-reference bool))

(cffi:defcstruct instance-binding-callbacks
  (create-callback instance-binding-create-callback)
  (free-callback instance-binding-free-callback)
  (reference-callback instance-binding-reference-callback))
(cffi:defctype instance-binding-callbacks (:struct instance-binding-callbacks))

(cffi:defctype class-instance-ptr (:pointer :void))

(cffi:defctype class-set (:pointer :void))

(defcfunproto class-set bool (p-instance class-instance-ptr)
 (p-name const-string-name-ptr) (p-value const-variant-ptr))

(cffi:defctype class-get (:pointer :void))

(defcfunproto class-get bool (p-instance class-instance-ptr)
 (p-name const-string-name-ptr) (r-ret variant-ptr))

(cffi:defctype class-get-rid (:pointer :void))

(defcfunproto class-get-rid :uint64 (p-instance class-instance-ptr))

(cffi:defcstruct property-info
  (type variant-type)
  (name string-name-ptr)
  (class-name string-name-ptr)
  (hint :uint32)
  (hint-string string-ptr)
  (usage :uint32))
(cffi:defctype property-info (:struct property-info))

(cffi:defcstruct method-info
  (name string-name-ptr)
  (return-value property-info)
  (flags :uint32)
  (id :int32)
  (argument-count :uint32)
  (arguments (:pointer property-info))
  (default-argument-count :uint32)
  (default-arguments (:pointer variant-ptr)))
(cffi:defctype method-info (:struct method-info))

(cffi:defctype class-get-property-list (:pointer :void))

(defcfunproto class-get-property-list (:pointer property-info)
 (p-instance class-instance-ptr) (r-count (:pointer :uint32)))

(cffi:defctype class-free-property-list (:pointer :void))

(defcfunproto class-free-property-list :void (p-instance class-instance-ptr)
 (p-list (:pointer property-info)))

(cffi:defctype class-free-property-list-2 (:pointer :void))

(defcfunproto class-free-property-list-2 :void (p-instance class-instance-ptr)
 (p-list (:pointer property-info)) (p-count :uint32))

(cffi:defctype class-property-can-revert (:pointer :void))

(defcfunproto class-property-can-revert bool (p-instance class-instance-ptr)
 (p-name const-string-name-ptr))

(cffi:defctype class-property-get-revert (:pointer :void))

(defcfunproto class-property-get-revert bool (p-instance class-instance-ptr)
 (p-name const-string-name-ptr) (r-ret variant-ptr))

(cffi:defctype class-validate-property (:pointer :void))

(defcfunproto class-validate-property bool (p-instance class-instance-ptr)
 (p-property (:pointer property-info)))

(cffi:defctype class-notification (:pointer :void) "DEPRECATED since 4.2")

(defcfunproto class-notification :void (p-instance class-instance-ptr)
 (p-what :int32))

(cffi:defctype class-notification-2 (:pointer :void))

(defcfunproto class-notification-2 :void (p-instance class-instance-ptr)
 (p-what :int32) (p-reversed bool))

(cffi:defctype class-to-string (:pointer :void))

(defcfunproto class-to-string :void (p-instance class-instance-ptr)
 (r-is-valid (:pointer bool)) (p-out string-ptr))

(cffi:defctype class-reference (:pointer :void))

(defcfunproto class-reference :void (p-instance class-instance-ptr))

(cffi:defctype class-unreference (:pointer :void))

(defcfunproto class-unreference :void (p-instance class-instance-ptr))

(cffi:defctype class-call-virtual (:pointer :void))

(defcfunproto class-call-virtual :void (p-instance class-instance-ptr)
 (p-args (:pointer const-type-ptr)) (r-ret type-ptr))

(cffi:defctype class-create-instance (:pointer :void))

(defcfunproto class-create-instance object-ptr
 (p-class-userdata (:pointer :void)))

(cffi:defctype class-create-instance-2 (:pointer :void))

(defcfunproto class-create-instance-2 object-ptr
 (p-class-userdata (:pointer :void)) (p-notify-postinitialize bool))

(cffi:defctype class-free-instance (:pointer :void))

(defcfunproto class-free-instance :void (p-class-userdata (:pointer :void))
 (p-instance class-instance-ptr))

(cffi:defctype class-recreate-instance (:pointer :void))

(defcfunproto class-recreate-instance class-instance-ptr
 (p-class-userdata (:pointer :void)) (p-object object-ptr))

(cffi:defctype class-get-virtual (:pointer :void))

(defcfunproto class-get-virtual class-call-virtual
 (p-class-userdata (:pointer :void)) (p-name const-string-name-ptr))

(cffi:defctype class-get-virtual-2 (:pointer :void))

(defcfunproto class-get-virtual-2 class-call-virtual
 (p-class-userdata (:pointer :void)) (p-name const-string-name-ptr)
 (p-hash :uint32))

(cffi:defctype class-get-virtual-call-data (:pointer :void))

(defcfunproto class-get-virtual-call-data (:pointer :void)
 (p-class-userdata (:pointer :void)) (p-name const-string-name-ptr))

(cffi:defctype class-get-virtual-call-data-2 (:pointer :void))

(defcfunproto class-get-virtual-call-data-2 (:pointer :void)
 (p-class-userdata (:pointer :void)) (p-name const-string-name-ptr)
 (p-hash :uint32))

(cffi:defctype class-call-virtual-with-data (:pointer :void))

(defcfunproto class-call-virtual-with-data :void
 (p-instance class-instance-ptr) (p-name const-string-name-ptr)
 (p-virtual-call-userdata (:pointer :void)) (p-args (:pointer const-type-ptr))
 (r-ret type-ptr))

(cffi:defcstruct class-creation-info
  "DEPRECATED since 4.2"
  (is-virtual bool)
  (is-abstract bool)
  (set-func class-set)
  (get-func class-get)
  (get-property-list-func class-get-property-list)
  (free-property-list-func class-free-property-list)
  (property-can-revert-func class-property-can-revert)
  (property-get-revert-func class-property-get-revert)
  (notification-func class-notification)
  (to-string-func class-to-string)
  (reference-func class-reference)
  (unreference-func class-unreference)
  (create-instance-func class-create-instance)
  (free-instance-func class-free-instance)
  (get-virtual-func class-get-virtual)
  (get-rid-func class-get-rid)
  (class-userdata (:pointer :void)))
(cffi:defctype class-creation-info (:struct class-creation-info))

(cffi:defcstruct class-creation-info-2
  "DEPRECATED since 4.3"
  (is-virtual bool)
  (is-abstract bool)
  (is-exposed bool)
  (set-func class-set)
  (get-func class-get)
  (get-property-list-func class-get-property-list)
  (free-property-list-func class-free-property-list)
  (property-can-revert-func class-property-can-revert)
  (property-get-revert-func class-property-get-revert)
  (validate-property-func class-validate-property)
  (notification-func class-notification-2)
  (to-string-func class-to-string)
  (reference-func class-reference)
  (unreference-func class-unreference)
  (create-instance-func class-create-instance)
  (free-instance-func class-free-instance)
  (recreate-instance-func class-recreate-instance)
  (get-virtual-func class-get-virtual)
  (get-virtual-call-data-func class-get-virtual-call-data)
  (call-virtual-with-data-func class-call-virtual-with-data)
  (get-rid-func class-get-rid)
  (class-userdata (:pointer :void)))
(cffi:defctype class-creation-info-2 (:struct class-creation-info-2))

(cffi:defcstruct class-creation-info-3
  "DEPRECATED since 4.4"
  (is-virtual bool)
  (is-abstract bool)
  (is-exposed bool)
  (is-runtime bool)
  (set-func class-set)
  (get-func class-get)
  (get-property-list-func class-get-property-list)
  (free-property-list-func class-free-property-list-2)
  (property-can-revert-func class-property-can-revert)
  (property-get-revert-func class-property-get-revert)
  (validate-property-func class-validate-property)
  (notification-func class-notification-2)
  (to-string-func class-to-string)
  (reference-func class-reference)
  (unreference-func class-unreference)
  (create-instance-func class-create-instance)
  (free-instance-func class-free-instance)
  (recreate-instance-func class-recreate-instance)
  (get-virtual-func class-get-virtual)
  (get-virtual-call-data-func class-get-virtual-call-data)
  (call-virtual-with-data-func class-call-virtual-with-data)
  (get-rid-func class-get-rid)
  (class-userdata (:pointer :void)))
(cffi:defctype class-creation-info-3 (:struct class-creation-info-3))

(cffi:defcstruct class-creation-info-4
  (is-virtual bool)
  (is-abstract bool)
  (is-exposed bool)
  (is-runtime bool)
  (icon-path const-string-ptr)
  (set-func class-set)
  (get-func class-get)
  (get-property-list-func class-get-property-list)
  (free-property-list-func class-free-property-list-2)
  (property-can-revert-func class-property-can-revert)
  (property-get-revert-func class-property-get-revert)
  (validate-property-func class-validate-property)
  (notification-func class-notification-2)
  (to-string-func class-to-string)
  (reference-func class-reference)
  (unreference-func class-unreference)
  (create-instance-func class-create-instance-2)
  (free-instance-func class-free-instance)
  (recreate-instance-func class-recreate-instance)
  (get-virtual-func class-get-virtual-2)
  (get-virtual-call-data-func class-get-virtual-call-data-2)
  (call-virtual-with-data-func class-call-virtual-with-data)
  (class-userdata (:pointer :void)))
(cffi:defctype class-creation-info-4 (:struct class-creation-info-4))

(cffi:defctype class-creation-info-5 class-creation-info-4)

(cffi:defctype class-library-ptr (:pointer :void))

(cffi:defctype editor-get-classes-used-callback (:pointer :void)
               "Passed a pointer to a PackedStringArray that should be filled with the classes that may be used by the GDExtension.")

(defcfunproto editor-get-classes-used-callback :void
 (p-packed-string-array type-ptr))

(cffi:defbitfield class-method-flags
  (:flag-normal 1)
  (:flag-editor 2)
  (:flag-const 4)
  (:flag-virtual 8)
  (:flag-vararg 16)
  (:flag-static 32)
  (:flag-virtual-required 128)
  (:flags-default 1))

(cffi:defcenum class-method-argument-metadata
  (:none 0)
  (:int-is-int8 1)
  (:int-is-int16 2)
  (:int-is-int32 3)
  (:int-is-int64 4)
  (:int-is-uint8 5)
  (:int-is-uint16 6)
  (:int-is-uint32 7)
  (:int-is-uint64 8)
  (:real-is-float 9)
  (:real-is-double 10)
  (:int-is-char16 11)
  (:int-is-char32 12)
  (:object-is-required 13))

(cffi:defctype class-method-call (:pointer :void))

(defcfunproto class-method-call :void (method-userdata (:pointer :void))
 (p-instance class-instance-ptr) (p-args (:pointer const-variant-ptr))
 (p-argument-count int) (r-return variant-ptr) (r-error (:pointer call-error)))

(cffi:defctype class-method-validated-call (:pointer :void))

(defcfunproto class-method-validated-call :void
 (method-userdata (:pointer :void)) (p-instance class-instance-ptr)
 (p-args (:pointer const-variant-ptr)) (r-return variant-ptr))

(cffi:defctype class-method-ptr-call (:pointer :void))

(defcfunproto class-method-ptr-call :void (method-userdata (:pointer :void))
 (p-instance class-instance-ptr) (p-args (:pointer const-type-ptr))
 (r-ret type-ptr))

(cffi:defcstruct class-method-info
  (name string-name-ptr)
  (method-userdata (:pointer :void))
  (call-func class-method-call)
  (ptrcall-func class-method-ptr-call)
  (method-flags :uint32)
  (has-return-value bool)
  (return-value-info (:pointer property-info))
  (return-value-metadata class-method-argument-metadata)
  (argument-count :uint32)
  (arguments-info (:pointer property-info))
  (arguments-metadata (:pointer class-method-argument-metadata))
  (default-argument-count :uint32)
  (default-arguments (:pointer variant-ptr)))
(cffi:defctype class-method-info (:struct class-method-info))

(cffi:defcstruct class-virtual-method-info
  (name string-name-ptr)
  (method-flags :uint32)
  (return-value property-info)
  (return-value-metadata class-method-argument-metadata)
  (argument-count :uint32)
  (arguments (:pointer property-info))
  (arguments-metadata (:pointer class-method-argument-metadata)))
(cffi:defctype class-virtual-method-info (:struct class-virtual-method-info))

(cffi:defctype callable-custom-call (:pointer :void))

(defcfunproto callable-custom-call :void (callable-userdata (:pointer :void))
 (p-args (:pointer const-variant-ptr)) (p-argument-count int)
 (r-return variant-ptr) (r-error (:pointer call-error)))

(cffi:defctype callable-custom-is-valid (:pointer :void))

(defcfunproto callable-custom-is-valid bool
 (callable-userdata (:pointer :void)))

(cffi:defctype callable-custom-free (:pointer :void))

(defcfunproto callable-custom-free :void (callable-userdata (:pointer :void)))

(cffi:defctype callable-custom-hash (:pointer :void))

(defcfunproto callable-custom-hash :uint32 (callable-userdata (:pointer :void)))

(cffi:defctype callable-custom-equal (:pointer :void))

(defcfunproto callable-custom-equal bool (callable-userdata-a (:pointer :void))
 (callable-userdata-b (:pointer :void)))

(cffi:defctype callable-custom-less-than (:pointer :void))

(defcfunproto callable-custom-less-than bool
 (callable-userdata-a (:pointer :void)) (callable-userdata-b (:pointer :void)))

(cffi:defctype callable-custom-to-string (:pointer :void))

(defcfunproto callable-custom-to-string :void
 (callable-userdata (:pointer :void)) (r-is-valid (:pointer bool))
 (r-out string-ptr))

(cffi:defctype callable-custom-get-argument-count (:pointer :void))

(defcfunproto callable-custom-get-argument-count int
 (callable-userdata (:pointer :void)) (r-is-valid (:pointer bool)))

(cffi:defcstruct callable-custom-info
  "DEPRECATED since 4.3
Only `call_func` and `token` are strictly required, however, `object_id` should be passed if its not a static method.
`token` should point to an address that uniquely identifies the GDExtension (for example, the
`GDExtensionClassLibraryPtr` passed to the entry symbol function.
`hash_func`, `equal_func`, and `less_than_func` are optional. If not provided both `call_func` and
`callable_userdata` together are used as the identity of the callable for hashing and comparison purposes.
The hash returned by `hash_func` is cached, `hash_func` will not be called more than once per callable.
`is_valid_func` is necessary if the validity of the callable can change before destruction.
`free_func` is necessary if `callable_userdata` needs to be cleaned up when the callable is freed."
  (callable-userdata (:pointer :void))
  (token (:pointer :void))
  (object-id instance-id)
  (call-func callable-custom-call)
  (is-valid-func callable-custom-is-valid)
  (free-func callable-custom-free)
  (hash-func callable-custom-hash)
  (equal-func callable-custom-equal)
  (less-than-func callable-custom-less-than)
  (to-string-func callable-custom-to-string))
(cffi:defctype callable-custom-info (:struct callable-custom-info))

(cffi:defcstruct callable-custom-info-2
  "Only `call_func` and `token` are strictly required, however, `object_id` should be passed if its not a static method.
`token` should point to an address that uniquely identifies the GDExtension (for example, the
`GDExtensionClassLibraryPtr` passed to the entry symbol function.
`hash_func`, `equal_func`, and `less_than_func` are optional. If not provided both `call_func` and
`callable_userdata` together are used as the identity of the callable for hashing and comparison purposes.
The hash returned by `hash_func` is cached, `hash_func` will not be called more than once per callable.
`is_valid_func` is necessary if the validity of the callable can change before destruction.
`free_func` is necessary if `callable_userdata` needs to be cleaned up when the callable is freed."
  (callable-userdata (:pointer :void))
  (token (:pointer :void))
  (object-id instance-id)
  (call-func callable-custom-call)
  (is-valid-func callable-custom-is-valid)
  (free-func callable-custom-free)
  (hash-func callable-custom-hash)
  (equal-func callable-custom-equal)
  (less-than-func callable-custom-less-than)
  (to-string-func callable-custom-to-string)
  (get-argument-count-func callable-custom-get-argument-count))
(cffi:defctype callable-custom-info-2 (:struct callable-custom-info-2))

(cffi:defctype script-instance-data-ptr (:pointer :void)
               "Pointer to custom ScriptInstance native implementation.")

(cffi:defctype script-instance-set (:pointer :void))

(defcfunproto script-instance-set bool (p-instance script-instance-data-ptr)
 (p-name const-string-name-ptr) (p-value const-variant-ptr))

(cffi:defctype script-instance-get (:pointer :void))

(defcfunproto script-instance-get bool (p-instance script-instance-data-ptr)
 (p-name const-string-name-ptr) (r-ret variant-ptr))

(cffi:defctype script-instance-get-property-list (:pointer :void))

(defcfunproto script-instance-get-property-list (:pointer property-info)
 (p-instance script-instance-data-ptr) (r-count (:pointer :uint32)))

(cffi:defctype script-instance-free-property-list (:pointer :void)
               "DEPRECATED since 4.3")

(defcfunproto script-instance-free-property-list :void
 (p-instance script-instance-data-ptr) (p-list (:pointer property-info)))

(cffi:defctype script-instance-free-property-list-2 (:pointer :void))

(defcfunproto script-instance-free-property-list-2 :void
 (p-instance script-instance-data-ptr) (p-list (:pointer property-info))
 (p-count :uint32))

(cffi:defctype script-instance-get-class-category (:pointer :void))

(defcfunproto script-instance-get-class-category bool
 (p-instance script-instance-data-ptr)
 (p-class-category (:pointer property-info)))

(cffi:defctype script-instance-get-property-type (:pointer :void))

(defcfunproto script-instance-get-property-type variant-type
 (p-instance script-instance-data-ptr) (p-name const-string-name-ptr)
 (r-is-valid (:pointer bool)))

(cffi:defctype script-instance-validate-property (:pointer :void))

(defcfunproto script-instance-validate-property bool
 (p-instance script-instance-data-ptr) (p-property (:pointer property-info)))

(cffi:defctype script-instance-property-can-revert (:pointer :void))

(defcfunproto script-instance-property-can-revert bool
 (p-instance script-instance-data-ptr) (p-name const-string-name-ptr))

(cffi:defctype script-instance-property-get-revert (:pointer :void))

(defcfunproto script-instance-property-get-revert bool
 (p-instance script-instance-data-ptr) (p-name const-string-name-ptr)
 (r-ret variant-ptr))

(cffi:defctype script-instance-get-owner (:pointer :void))

(defcfunproto script-instance-get-owner object-ptr
 (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-property-state-add (:pointer :void))

(defcfunproto script-instance-property-state-add :void
 (p-name const-string-name-ptr) (p-value const-variant-ptr)
 (p-userdata (:pointer :void)))

(cffi:defctype script-instance-get-property-state (:pointer :void))

(defcfunproto script-instance-get-property-state :void
 (p-instance script-instance-data-ptr)
 (p-add-func script-instance-property-state-add) (p-userdata (:pointer :void)))

(cffi:defctype script-instance-get-method-list (:pointer :void))

(defcfunproto script-instance-get-method-list (:pointer method-info)
 (p-instance script-instance-data-ptr) (r-count (:pointer :uint32)))

(cffi:defctype script-instance-free-method-list (:pointer :void)
               "DEPRECATED since 4.3")

(defcfunproto script-instance-free-method-list :void
 (p-instance script-instance-data-ptr) (p-list (:pointer method-info)))

(cffi:defctype script-instance-free-method-list-2 (:pointer :void))

(defcfunproto script-instance-free-method-list-2 :void
 (p-instance script-instance-data-ptr) (p-list (:pointer method-info))
 (p-count :uint32))

(cffi:defctype script-instance-has-method (:pointer :void))

(defcfunproto script-instance-has-method bool
 (p-instance script-instance-data-ptr) (p-name const-string-name-ptr))

(cffi:defctype script-instance-get-method-argument-count (:pointer :void))

(defcfunproto script-instance-get-method-argument-count int
 (p-instance script-instance-data-ptr) (p-name const-string-name-ptr)
 (r-is-valid (:pointer bool)))

(cffi:defctype script-instance-call (:pointer :void))

(defcfunproto script-instance-call :void (p-self script-instance-data-ptr)
 (p-method const-string-name-ptr) (p-args (:pointer const-variant-ptr))
 (p-argument-count int) (r-return variant-ptr) (r-error (:pointer call-error)))

(cffi:defctype script-instance-notification (:pointer :void)
               "DEPRECATED since 4.2")

(defcfunproto script-instance-notification :void
 (p-instance script-instance-data-ptr) (p-what :int32))

(cffi:defctype script-instance-notification-2 (:pointer :void))

(defcfunproto script-instance-notification-2 :void
 (p-instance script-instance-data-ptr) (p-what :int32) (p-reversed bool))

(cffi:defctype script-instance-to-string (:pointer :void))

(defcfunproto script-instance-to-string :void
 (p-instance script-instance-data-ptr) (r-is-valid (:pointer bool))
 (r-out string-ptr))

(cffi:defctype script-instance-ref-count-incremented (:pointer :void))

(defcfunproto script-instance-ref-count-incremented :void
 (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-ref-count-decremented (:pointer :void))

(defcfunproto script-instance-ref-count-decremented bool
 (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-get-script (:pointer :void))

(defcfunproto script-instance-get-script object-ptr
 (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-is-placeholder (:pointer :void))

(defcfunproto script-instance-is-placeholder bool
 (p-instance script-instance-data-ptr))

(cffi:defctype script-language-ptr (:pointer :void))

(cffi:defctype script-instance-get-language (:pointer :void))

(defcfunproto script-instance-get-language script-language-ptr
 (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-free (:pointer :void))

(defcfunproto script-instance-free :void (p-instance script-instance-data-ptr))

(cffi:defctype script-instance-ptr (:pointer :void)
               "Pointer to ScriptInstance.")

(cffi:defcstruct script-instance-info
  "DEPRECATED since 4.2"
  (set-func script-instance-set)
  (get-func script-instance-get)
  (get-property-list-func script-instance-get-property-list)
  (free-property-list-func script-instance-free-property-list)
  (property-can-revert-func script-instance-property-can-revert)
  (property-get-revert-func script-instance-property-get-revert)
  (get-owner-func script-instance-get-owner)
  (get-property-state-func script-instance-get-property-state)
  (get-method-list-func script-instance-get-method-list)
  (free-method-list-func script-instance-free-method-list)
  (get-property-type-func script-instance-get-property-type)
  (has-method-func script-instance-has-method)
  (call-func script-instance-call)
  (notification-func script-instance-notification)
  (to-string-func script-instance-to-string)
  (refcount-incremented-func script-instance-ref-count-incremented)
  (refcount-decremented-func script-instance-ref-count-decremented)
  (get-script-func script-instance-get-script)
  (is-placeholder-func script-instance-is-placeholder)
  (set-fallback-func script-instance-set)
  (get-fallback-func script-instance-get)
  (get-language-func script-instance-get-language)
  (free-func script-instance-free))
(cffi:defctype script-instance-info (:struct script-instance-info))

(cffi:defcstruct script-instance-info-2
  "DEPRECATED since 4.3"
  (set-func script-instance-set)
  (get-func script-instance-get)
  (get-property-list-func script-instance-get-property-list)
  (free-property-list-func script-instance-free-property-list)
  (get-class-category-func script-instance-get-class-category)
  (property-can-revert-func script-instance-property-can-revert)
  (property-get-revert-func script-instance-property-get-revert)
  (get-owner-func script-instance-get-owner)
  (get-property-state-func script-instance-get-property-state)
  (get-method-list-func script-instance-get-method-list)
  (free-method-list-func script-instance-free-method-list)
  (get-property-type-func script-instance-get-property-type)
  (validate-property-func script-instance-validate-property)
  (has-method-func script-instance-has-method)
  (call-func script-instance-call)
  (notification-func script-instance-notification-2)
  (to-string-func script-instance-to-string)
  (refcount-incremented-func script-instance-ref-count-incremented)
  (refcount-decremented-func script-instance-ref-count-decremented)
  (get-script-func script-instance-get-script)
  (is-placeholder-func script-instance-is-placeholder)
  (set-fallback-func script-instance-set)
  (get-fallback-func script-instance-get)
  (get-language-func script-instance-get-language)
  (free-func script-instance-free))
(cffi:defctype script-instance-info-2 (:struct script-instance-info-2))

(cffi:defcstruct script-instance-info-3
  (set-func script-instance-set)
  (get-func script-instance-get)
  (get-property-list-func script-instance-get-property-list)
  (free-property-list-func script-instance-free-property-list-2)
  (get-class-category-func script-instance-get-class-category)
  (property-can-revert-func script-instance-property-can-revert)
  (property-get-revert-func script-instance-property-get-revert)
  (get-owner-func script-instance-get-owner)
  (get-property-state-func script-instance-get-property-state)
  (get-method-list-func script-instance-get-method-list)
  (free-method-list-func script-instance-free-method-list-2)
  (get-property-type-func script-instance-get-property-type)
  (validate-property-func script-instance-validate-property)
  (has-method-func script-instance-has-method)
  (get-method-argument-count-func script-instance-get-method-argument-count)
  (call-func script-instance-call)
  (notification-func script-instance-notification-2)
  (to-string-func script-instance-to-string)
  (refcount-incremented-func script-instance-ref-count-incremented)
  (refcount-decremented-func script-instance-ref-count-decremented)
  (get-script-func script-instance-get-script)
  (is-placeholder-func script-instance-is-placeholder)
  (set-fallback-func script-instance-set)
  (get-fallback-func script-instance-get)
  (get-language-func script-instance-get-language)
  (free-func script-instance-free))
(cffi:defctype script-instance-info-3 (:struct script-instance-info-3))

(cffi:defctype worker-thread-pool-group-task (:pointer :void))

(defcfunproto worker-thread-pool-group-task :void (|| (:pointer :void))
 (arg1 :uint32))

(cffi:defctype worker-thread-pool-task (:pointer :void))

(defcfunproto worker-thread-pool-task :void (|| (:pointer :void)))

(cffi:defcenum initialization-level
  (:initialization-core 0)
  (:initialization-servers 1)
  (:initialization-scene 2)
  (:initialization-editor 3)
  (:max-initialization-level 4))

(cffi:defctype initialize-callback (:pointer :void))

(defcfunproto initialize-callback :void (p-userdata (:pointer :void))
 (p-level initialization-level))

(cffi:defctype deinitialize-callback (:pointer :void))

(defcfunproto deinitialize-callback :void (p-userdata (:pointer :void))
 (p-level initialization-level))

(cffi:defcstruct initialization
  (minimum-initialization-level initialization-level)
  (userdata (:pointer :void))
  (initialize initialize-callback)
  (deinitialize deinitialize-callback))
(cffi:defctype initialization (:struct initialization))

(cffi:defctype interface-function-ptr (:pointer :void))

(defcfunproto interface-function-ptr :void)

(cffi:defctype interface-get-proc-address (:pointer :void))

(defcfunproto interface-get-proc-address interface-function-ptr
 (p-function-name (:pointer :char)))

(cffi:defctype initialization-function (:pointer :void)
               "Each GDExtension should define a C function that matches the signature of GDExtensionInitializationFunction,
and export it so that it can be loaded via dlopen() or equivalent for the given platform.
For example:
  GDExtensionBool my_extension_init(GDExtensionInterfaceGetProcAddress p_get_proc_address, GDExtensionClassLibraryPtr p_library, GDExtensionInitialization *r_initialization);
This function's name must be specified as the 'entry_symbol' in the .gdextension file.
This makes it the entry point of the GDExtension and will be called on initialization.
The GDExtension can then modify the r_initialization structure, setting the minimum initialization level,
and providing pointers to functions that will be called at various stages of initialization/shutdown.
The rest of the GDExtension's interface to Godot consists of function pointers that can be loaded
by calling p_get_proc_address(\"...\") with the name of the function.
For example:
  GDExtensionInterfaceGetGodotVersion get_godot_version = (GDExtensionInterfaceGetGodotVersion)p_get_proc_address(\"get_godot_version\");
(Note that snippet may cause \"cast between incompatible function types\" on some compilers, you can
silence this by adding an intermediary `void*` cast.)
You can then call it like a normal function:
  GDExtensionGodotVersion godot_version;
  get_godot_version(&godot_version);
  printf(\"Godot v%d.%d.%d\\n\", godot_version.major, godot_version.minor, godot_version.patch);
All of these interface functions are described below, together with the name that's used to load it,
and the function pointer typedef that shows its signature.")

(defcfunproto initialization-function bool
 (p-get-proc-address interface-get-proc-address) (p-library class-library-ptr)
 (r-initialization (:pointer initialization)))

(cffi:defcstruct godot-version
  (major :uint32)
  (minor :uint32)
  (patch :uint32)
  (string (:pointer :char)))
(cffi:defctype godot-version (:struct godot-version))

(cffi:defcstruct godot-version-2
  (major :uint32)
  (minor :uint32)
  (patch :uint32)
  (hex :uint32)
  (status (:pointer :char))
  (build (:pointer :char))
  (hash (:pointer :char))
  (timestamp :uint64)
  (string (:pointer :char)))
(cffi:defctype godot-version-2 (:struct godot-version-2))

(cffi:defctype main-loop-startup-callback (:pointer :void)
               "Called when starting the main loop.")

(defcfunproto main-loop-startup-callback :void)

(cffi:defctype main-loop-shutdown-callback (:pointer :void)
               "Called when shutting down the main loop.")

(defcfunproto main-loop-shutdown-callback :void)

(cffi:defctype main-loop-frame-callback (:pointer :void)
               "Called for every frame iteration of the main loop.")

(defcfunproto main-loop-frame-callback :void)

(cffi:defcstruct main-loop-callbacks
  (startup-func main-loop-startup-callback)
  (shutdown-func main-loop-shutdown-callback)
  (frame-func main-loop-frame-callback))
(cffi:defctype main-loop-callbacks (:struct main-loop-callbacks))