(common-lisp:in-package :%godot)


(defgmethod (dictionary+size :class 'dictionary :bind "size" :hash 3173160232)
 int)

(defgmethod
 (dictionary+is-empty :class 'dictionary :bind "is_empty" :hash 3918633141)
 bool)

(defgmethod
 (dictionary+clear :class 'dictionary :bind "clear" :hash 3218959716) :void)

(defgmethod
 (dictionary+assign :class 'dictionary :bind "assign" :hash 3642266950) :void
 (dictionary dictionary))

(defgmethod (dictionary+sort :class 'dictionary :bind "sort" :hash 3218959716)
 :void)

(defgmethod
 (dictionary+merge :class 'dictionary :bind "merge" :hash 2079548978) :void
 (dictionary dictionary) (overwrite bool))

(defgmethod
 (dictionary+merged :class 'dictionary :bind "merged" :hash 2271165639)
 dictionary (dictionary dictionary) (overwrite bool))

(defgmethod (dictionary+has :class 'dictionary :bind "has" :hash 3680194679)
 bool (key variant))

(defgmethod
 (dictionary+has-all :class 'dictionary :bind "has_all" :hash 2988181878) bool
 (keys array))

(defgmethod
 (dictionary+find-key :class 'dictionary :bind "find_key" :hash 1988825835)
 variant (value variant))

(defgmethod
 (dictionary+erase :class 'dictionary :bind "erase" :hash 1776646889) bool
 (key variant))

(defgmethod (dictionary+hash :class 'dictionary :bind "hash" :hash 3173160232)
 int)

(defgmethod (dictionary+keys :class 'dictionary :bind "keys" :hash 4144163970)
 array)

(defgmethod
 (dictionary+values :class 'dictionary :bind "values" :hash 4144163970) array)

(defgmethod
 (dictionary+duplicate :class 'dictionary :bind "duplicate" :hash 830099069)
 dictionary (deep bool))

(defgmethod
 (dictionary+duplicate-deep :class 'dictionary :bind "duplicate_deep" :hash
  2160600714)
 dictionary (deep-subresources-mode int))

(defgmethod (dictionary+get :class 'dictionary :bind "get" :hash 2205440559)
 variant (key variant) (default variant))

(defgmethod
 (dictionary+get-or-add :class 'dictionary :bind "get_or_add" :hash 1052551076)
 variant (key variant) (default variant))

(defgmethod (dictionary+set :class 'dictionary :bind "set" :hash 2175348267)
 bool (key variant) (value variant))

(defgmethod
 (dictionary+is-typed :class 'dictionary :bind "is_typed" :hash 3918633141)
 bool)

(defgmethod
 (dictionary+is-typed-key :class 'dictionary :bind "is_typed_key" :hash
  3918633141)
 bool)

(defgmethod
 (dictionary+is-typed-value :class 'dictionary :bind "is_typed_value" :hash
  3918633141)
 bool)

(defgmethod
 (dictionary+is-same-typed :class 'dictionary :bind "is_same_typed" :hash
  3471775634)
 bool (dictionary dictionary))

(defgmethod
 (dictionary+is-same-typed-key :class 'dictionary :bind "is_same_typed_key"
  :hash 3471775634)
 bool (dictionary dictionary))

(defgmethod
 (dictionary+is-same-typed-value :class 'dictionary :bind "is_same_typed_value"
  :hash 3471775634)
 bool (dictionary dictionary))

(defgmethod
 (dictionary+get-typed-key-builtin :class 'dictionary :bind
  "get_typed_key_builtin" :hash 3173160232)
 int)

(defgmethod
 (dictionary+get-typed-value-builtin :class 'dictionary :bind
  "get_typed_value_builtin" :hash 3173160232)
 int)

(defgmethod
 (dictionary+get-typed-key-class-name :class 'dictionary :bind
  "get_typed_key_class_name" :hash 1825232092)
 string-name)

(defgmethod
 (dictionary+get-typed-value-class-name :class 'dictionary :bind
  "get_typed_value_class_name" :hash 1825232092)
 string-name)

(defgmethod
 (dictionary+get-typed-key-script :class 'dictionary :bind
  "get_typed_key_script" :hash 1460142086)
 variant)

(defgmethod
 (dictionary+get-typed-value-script :class 'dictionary :bind
  "get_typed_value_script" :hash 1460142086)
 variant)

(defgmethod
 (dictionary+make-read-only :class 'dictionary :bind "make_read_only" :hash
  3218959716)
 :void)

(defgmethod
 (dictionary+is-read-only :class 'dictionary :bind "is_read_only" :hash
  3918633141)
 bool)

(defgmethod
 (dictionary+recursive-equal :class 'dictionary :bind "recursive_equal" :hash
  1404404751)
 bool (dictionary dictionary) (recursion-count int))