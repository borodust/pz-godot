(common-lisp:in-package :%godot)


(defgmethod
 (packed-int-64array+get :class 'packed-int-64array :bind "get" :hash
  4103005248)
 int (index int))

(defgmethod
 (packed-int-64array+set :class 'packed-int-64array :bind "set" :hash
  3638975848)
 :void (index int) (value int))

(defgmethod
 (packed-int-64array+size :class 'packed-int-64array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-int-64array+is-empty :class 'packed-int-64array :bind "is_empty" :hash
  3918633141)
 bool)

(defgmethod
 (packed-int-64array+push-back :class 'packed-int-64array :bind "push_back"
  :hash 694024632)
 bool (value int))

(defgmethod
 (packed-int-64array+append :class 'packed-int-64array :bind "append" :hash
  694024632)
 bool (value int))

(defgmethod
 (packed-int-64array+append-array :class 'packed-int-64array :bind
  "append_array" :hash 2090311302)
 :void (array packed-int-64array))

(defgmethod
 (packed-int-64array+remove-at :class 'packed-int-64array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-int-64array+insert :class 'packed-int-64array :bind "insert" :hash
  1487112728)
 int (at-index int) (value int))

(defgmethod
 (packed-int-64array+fill :class 'packed-int-64array :bind "fill" :hash
  2823966027)
 :void (value int))

(defgmethod
 (packed-int-64array+resize :class 'packed-int-64array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-int-64array+clear :class 'packed-int-64array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-64array+has :class 'packed-int-64array :bind "has" :hash
  931488181)
 bool (value int))

(defgmethod
 (packed-int-64array+reverse :class 'packed-int-64array :bind "reverse" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-64array+slice :class 'packed-int-64array :bind "slice" :hash
  1726550804)
 packed-int-64array (begin int) (end int))

(defgmethod
 (packed-int-64array+to-byte-array :class 'packed-int-64array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-int-64array+sort :class 'packed-int-64array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-64array+bsearch :class 'packed-int-64array :bind "bsearch" :hash
  954237325)
 int (value int) (before bool))

(defgmethod
 (packed-int-64array+duplicate :class 'packed-int-64array :bind "duplicate"
  :hash 1961294120)
 packed-int-64array)

(defgmethod
 (packed-int-64array+find :class 'packed-int-64array :bind "find" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-int-64array+rfind :class 'packed-int-64array :bind "rfind" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-int-64array+count :class 'packed-int-64array :bind "count" :hash
  4103005248)
 int (value int))

(defgmethod
 (packed-int-64array+erase :class 'packed-int-64array :bind "erase" :hash
  694024632)
 bool (value int))