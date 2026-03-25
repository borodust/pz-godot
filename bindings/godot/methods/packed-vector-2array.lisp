(common-lisp:in-package :%godot)


(defgmethod
 (packed-vector-2array+get :class 'packed-vector-2array :bind "get" :hash
  2609058838)
 vector-2 (index int))

(defgmethod
 (packed-vector-2array+set :class 'packed-vector-2array :bind "set" :hash
  635767250)
 :void (index int) (value vector-2))

(defgmethod
 (packed-vector-2array+size :class 'packed-vector-2array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-vector-2array+is-empty :class 'packed-vector-2array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-vector-2array+push-back :class 'packed-vector-2array :bind "push_back"
  :hash 4188891560)
 bool (value vector-2))

(defgmethod
 (packed-vector-2array+append :class 'packed-vector-2array :bind "append" :hash
  4188891560)
 bool (value vector-2))

(defgmethod
 (packed-vector-2array+append-array :class 'packed-vector-2array :bind
  "append_array" :hash 3887534835)
 :void (array packed-vector-2array))

(defgmethod
 (packed-vector-2array+remove-at :class 'packed-vector-2array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-vector-2array+insert :class 'packed-vector-2array :bind "insert" :hash
  2225629369)
 int (at-index int) (value vector-2))

(defgmethod
 (packed-vector-2array+fill :class 'packed-vector-2array :bind "fill" :hash
  3790411178)
 :void (value vector-2))

(defgmethod
 (packed-vector-2array+resize :class 'packed-vector-2array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-vector-2array+clear :class 'packed-vector-2array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-2array+has :class 'packed-vector-2array :bind "has" :hash
  3190634762)
 bool (value vector-2))

(defgmethod
 (packed-vector-2array+reverse :class 'packed-vector-2array :bind "reverse"
  :hash 3218959716)
 :void)

(defgmethod
 (packed-vector-2array+slice :class 'packed-vector-2array :bind "slice" :hash
  3864005350)
 packed-vector-2array (begin int) (end int))

(defgmethod
 (packed-vector-2array+to-byte-array :class 'packed-vector-2array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-vector-2array+sort :class 'packed-vector-2array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-2array+bsearch :class 'packed-vector-2array :bind "bsearch"
  :hash 3341588170)
 int (value vector-2) (before bool))

(defgmethod
 (packed-vector-2array+duplicate :class 'packed-vector-2array :bind "duplicate"
  :hash 1660374357)
 packed-vector-2array)

(defgmethod
 (packed-vector-2array+find :class 'packed-vector-2array :bind "find" :hash
  1469606149)
 int (value vector-2) (from int))

(defgmethod
 (packed-vector-2array+rfind :class 'packed-vector-2array :bind "rfind" :hash
  1469606149)
 int (value vector-2) (from int))

(defgmethod
 (packed-vector-2array+count :class 'packed-vector-2array :bind "count" :hash
  2798848307)
 int (value vector-2))

(defgmethod
 (packed-vector-2array+erase :class 'packed-vector-2array :bind "erase" :hash
  4188891560)
 bool (value vector-2))