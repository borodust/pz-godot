(common-lisp:in-package :%godot)


(defgmethod
 (reg-ex-match+get-subject :class 'reg-ex-match :bind "get_subject" :hash
  201670096)
 string)

(defgmethod
 (reg-ex-match+get-group-count :class 'reg-ex-match :bind "get_group_count"
  :hash 3905245786)
 int)

(defgmethod
 (reg-ex-match+get-names :class 'reg-ex-match :bind "get_names" :hash
  3102165223)
 dictionary)

(defgmethod
 (reg-ex-match+get-strings :class 'reg-ex-match :bind "get_strings" :hash
  1139954409)
 packed-string-array)

(defgmethod
 (reg-ex-match+get-string :class 'reg-ex-match :bind "get_string" :hash
  687115856)
 string (name variant))

(defgmethod
 (reg-ex-match+get-start :class 'reg-ex-match :bind "get_start" :hash
  490464691)
 int (name variant))

(defgmethod
 (reg-ex-match+get-end :class 'reg-ex-match :bind "get_end" :hash 490464691)
 int (name variant))