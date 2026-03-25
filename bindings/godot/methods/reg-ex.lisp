(common-lisp:in-package :%godot)


(defgmethod
 (reg-ex+create-from-string :class 'reg-ex :bind "create_from_string" :hash
  4249111514 :static common-lisp:t)
 reg-ex (pattern string) (show-error bool))

(defgmethod (reg-ex+clear :class 'reg-ex :bind "clear" :hash 3218959716) :void)

(defgmethod (reg-ex+compile :class 'reg-ex :bind "compile" :hash 3565188097)
 error (pattern string) (show-error bool))

(defgmethod (reg-ex+search :class 'reg-ex :bind "search" :hash 3365977994)
 reg-ex-match (subject string) (offset int) (end int))

(defgmethod
 (reg-ex+search-all :class 'reg-ex :bind "search_all" :hash 849021363) array
 (subject string) (offset int) (end int))

(defgmethod (reg-ex+sub :class 'reg-ex :bind "sub" :hash 54019702) string
 (subject string) (replacement string) (all bool) (offset int) (end int))

(defgmethod (reg-ex+is-valid :class 'reg-ex :bind "is_valid" :hash 36873697)
 bool)

(defgmethod
 (reg-ex+get-pattern :class 'reg-ex :bind "get_pattern" :hash 201670096) string)

(defgmethod
 (reg-ex+get-group-count :class 'reg-ex :bind "get_group_count" :hash
  3905245786)
 int)

(defgmethod
 (reg-ex+get-names :class 'reg-ex :bind "get_names" :hash 1139954409)
 packed-string-array)