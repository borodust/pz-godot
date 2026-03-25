(common-lisp:in-package :%godot)


(defgmethod
 (class-db+get-class-list :class 'class-db :bind "get_class_list" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (class-db+get-inheriters-from-class :class 'class-db :bind
  "get_inheriters_from_class" :hash 1761182771)
 packed-string-array (class string-name))

(defgmethod
 (class-db+get-parent-class :class 'class-db :bind "get_parent_class" :hash
  1965194235)
 string-name (class string-name))

(defgmethod
 (class-db+class-exists :class 'class-db :bind "class_exists" :hash 2619796661)
 bool (class string-name))

(defgmethod
 (class-db+is-parent-class :class 'class-db :bind "is_parent_class" :hash
  471820014)
 bool (class string-name) (inherits string-name))

(defgmethod
 (class-db+can-instantiate :class 'class-db :bind "can_instantiate" :hash
  2619796661)
 bool (class string-name))

(defgmethod
 (class-db+instantiate :class 'class-db :bind "instantiate" :hash 2760726917)
 variant (class string-name))

(defgmethod
 (class-db+class-get-api-type :class 'class-db :bind "class_get_api_type" :hash
  2475317043)
 class-db+apitype (class string-name))

(defgmethod
 (class-db+class-has-signal :class 'class-db :bind "class_has_signal" :hash
  471820014)
 bool (class string-name) (signal string-name))

(defgmethod
 (class-db+class-get-signal :class 'class-db :bind "class_get_signal" :hash
  3061114238)
 dictionary (class string-name) (signal string-name))

(defgmethod
 (class-db+class-get-signal-list :class 'class-db :bind "class_get_signal_list"
  :hash 3504980660)
 array (class string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-property-list :class 'class-db :bind
  "class_get_property_list" :hash 3504980660)
 array (class string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-property-getter :class 'class-db :bind
  "class_get_property_getter" :hash 3770832642)
 string-name (class string-name) (property string-name))

(defgmethod
 (class-db+class-get-property-setter :class 'class-db :bind
  "class_get_property_setter" :hash 3770832642)
 string-name (class string-name) (property string-name))

(defgmethod
 (class-db+class-get-property :class 'class-db :bind "class_get_property" :hash
  2498641674)
 variant (object object) (property string-name))

(defgmethod
 (class-db+class-set-property :class 'class-db :bind "class_set_property" :hash
  1690314931)
 error (object object) (property string-name) (value variant))

(defgmethod
 (class-db+class-get-property-default-value :class 'class-db :bind
  "class_get_property_default_value" :hash 2718203076)
 variant (class string-name) (property string-name))

(defgmethod
 (class-db+class-has-method :class 'class-db :bind "class_has_method" :hash
  3860701026)
 bool (class string-name) (method string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-method-argument-count :class 'class-db :bind
  "class_get_method_argument_count" :hash 3885694822)
 int (class string-name) (method string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-method-list :class 'class-db :bind "class_get_method_list"
  :hash 3504980660)
 array (class string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-call-static :class 'class-db :bind "class_call_static" :hash
  3344196419 :vararg common-lisp:t)
 variant (class string-name) (method string-name))

(defgmethod
 (class-db+class-get-integer-constant-list :class 'class-db :bind
  "class_get_integer_constant_list" :hash 3031669221)
 packed-string-array (class string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-has-integer-constant :class 'class-db :bind
  "class_has_integer_constant" :hash 471820014)
 bool (class string-name) (name string-name))

(defgmethod
 (class-db+class-get-integer-constant :class 'class-db :bind
  "class_get_integer_constant" :hash 2419549490)
 int (class string-name) (name string-name))

(defgmethod
 (class-db+class-has-enum :class 'class-db :bind "class_has_enum" :hash
  3860701026)
 bool (class string-name) (name string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-enum-list :class 'class-db :bind "class_get_enum_list"
  :hash 3031669221)
 packed-string-array (class string-name) (no-inheritance bool))

(defgmethod
 (class-db+class-get-enum-constants :class 'class-db :bind
  "class_get_enum_constants" :hash 661528303)
 packed-string-array (class string-name) (enum string-name)
 (no-inheritance bool))

(defgmethod
 (class-db+class-get-integer-constant-enum :class 'class-db :bind
  "class_get_integer_constant_enum" :hash 2457504236)
 string-name (class string-name) (name string-name) (no-inheritance bool))

(defgmethod
 (class-db+is-class-enum-bitfield :class 'class-db :bind
  "is_class_enum_bitfield" :hash 3860701026)
 bool (class string-name) (enum string-name) (no-inheritance bool))

(defgmethod
 (class-db+is-class-enabled :class 'class-db :bind "is_class_enabled" :hash
  2619796661)
 bool (class string-name))