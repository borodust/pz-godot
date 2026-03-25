(common-lisp:in-package :%godot)


(defgmethod
 (node-path+is-absolute :class 'node-path :bind "is_absolute" :hash 3918633141)
 bool)

(defgmethod
 (node-path+get-name-count :class 'node-path :bind "get_name_count" :hash
  3173160232)
 int)

(defgmethod
 (node-path+get-name :class 'node-path :bind "get_name" :hash 2948586938)
 string-name (idx int))

(defgmethod
 (node-path+get-subname-count :class 'node-path :bind "get_subname_count" :hash
  3173160232)
 int)

(defgmethod (node-path+hash :class 'node-path :bind "hash" :hash 3173160232)
 int)

(defgmethod
 (node-path+get-subname :class 'node-path :bind "get_subname" :hash 2948586938)
 string-name (idx int))

(defgmethod
 (node-path+get-concatenated-names :class 'node-path :bind
  "get_concatenated_names" :hash 1825232092)
 string-name)

(defgmethod
 (node-path+get-concatenated-subnames :class 'node-path :bind
  "get_concatenated_subnames" :hash 1825232092)
 string-name)

(defgmethod (node-path+slice :class 'node-path :bind "slice" :hash 421628484)
 node-path (begin int) (end int))

(defgmethod
 (node-path+get-as-property-path :class 'node-path :bind "get_as_property_path"
  :hash 1598598043)
 node-path)

(defgmethod
 (node-path+is-empty :class 'node-path :bind "is_empty" :hash 3918633141) bool)