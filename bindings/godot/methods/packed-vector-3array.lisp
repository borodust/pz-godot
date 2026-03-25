(common-lisp:in-package :%godot)


(defgmethod
 (packed-vector-3array+get :class 'packed-vector-3array :bind "get" :hash
  1394941017)
 vector-3 (index int))

(defgmethod
 (packed-vector-3array+set :class 'packed-vector-3array :bind "set" :hash
  3975343409)
 :void (index int) (value vector-3))

(defgmethod
 (packed-vector-3array+size :class 'packed-vector-3array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-vector-3array+is-empty :class 'packed-vector-3array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-vector-3array+push-back :class 'packed-vector-3array :bind "push_back"
  :hash 3295363524)
 bool (value vector-3))

(defgmethod
 (packed-vector-3array+append :class 'packed-vector-3array :bind "append" :hash
  3295363524)
 bool (value vector-3))

(defgmethod
 (packed-vector-3array+append-array :class 'packed-vector-3array :bind
  "append_array" :hash 203538016)
 :void (array packed-vector-3array))

(defgmethod
 (packed-vector-3array+remove-at :class 'packed-vector-3array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-vector-3array+insert :class 'packed-vector-3array :bind "insert" :hash
  3892262309)
 int (at-index int) (value vector-3))

(defgmethod
 (packed-vector-3array+fill :class 'packed-vector-3array :bind "fill" :hash
  3726392409)
 :void (value vector-3))

(defgmethod
 (packed-vector-3array+resize :class 'packed-vector-3array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-vector-3array+clear :class 'packed-vector-3array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-3array+has :class 'packed-vector-3array :bind "has" :hash
  1749054343)
 bool (value vector-3))

(defgmethod
 (packed-vector-3array+reverse :class 'packed-vector-3array :bind "reverse"
  :hash 3218959716)
 :void)

(defgmethod
 (packed-vector-3array+slice :class 'packed-vector-3array :bind "slice" :hash
  2086131305)
 packed-vector-3array (begin int) (end int))

(defgmethod
 (packed-vector-3array+to-byte-array :class 'packed-vector-3array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-vector-3array+sort :class 'packed-vector-3array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-3array+bsearch :class 'packed-vector-3array :bind "bsearch"
  :hash 1259277637)
 int (value vector-3) (before bool))

(defgmethod
 (packed-vector-3array+duplicate :class 'packed-vector-3array :bind "duplicate"
  :hash 4171207452)
 packed-vector-3array)

(defgmethod
 (packed-vector-3array+find :class 'packed-vector-3array :bind "find" :hash
  3718155780)
 int (value vector-3) (from int))

(defgmethod
 (packed-vector-3array+rfind :class 'packed-vector-3array :bind "rfind" :hash
  3718155780)
 int (value vector-3) (from int))

(defgmethod
 (packed-vector-3array+count :class 'packed-vector-3array :bind "count" :hash
  194580386)
 int (value vector-3))

(defgmethod
 (packed-vector-3array+erase :class 'packed-vector-3array :bind "erase" :hash
  3295363524)
 bool (value vector-3))