(common-lisp:in-package :%godot)


(defgmethod
 (packed-int-32array+get :class 'packed-int-32array :bind "get" :hash
  4103005248)
 int (index int))

(defgmethod
 (packed-int-32array+set :class 'packed-int-32array :bind "set" :hash
  3638975848)
 :void (index int) (value int))

(defgmethod
 (packed-int-32array+size :class 'packed-int-32array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-int-32array+is-empty :class 'packed-int-32array :bind "is_empty" :hash
  3918633141)
 bool)

(defgmethod
 (packed-int-32array+push-back :class 'packed-int-32array :bind "push_back"
  :hash 694024632)
 bool (value int))

(defgmethod
 (packed-int-32array+append :class 'packed-int-32array :bind "append" :hash
  694024632)
 bool (value int))

(defgmethod
 (packed-int-32array+append-array :class 'packed-int-32array :bind
  "append_array" :hash 1087733270)
 :void (array packed-int-32array))

(defgmethod
 (packed-int-32array+remove-at :class 'packed-int-32array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-int-32array+insert :class 'packed-int-32array :bind "insert" :hash
  1487112728)
 int (at-index int) (value int))

(defgmethod
 (packed-int-32array+fill :class 'packed-int-32array :bind "fill" :hash
  2823966027)
 :void (value int))

(defgmethod
 (packed-int-32array+resize :class 'packed-int-32array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-int-32array+clear :class 'packed-int-32array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-32array+has :class 'packed-int-32array :bind "has" :hash
  931488181)
 bool (value int))

(defgmethod
 (packed-int-32array+reverse :class 'packed-int-32array :bind "reverse" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-32array+slice :class 'packed-int-32array :bind "slice" :hash
  1216021098)
 packed-int-32array (begin int) (end int))

(defgmethod
 (packed-int-32array+to-byte-array :class 'packed-int-32array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-int-32array+sort :class 'packed-int-32array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-int-32array+bsearch :class 'packed-int-32array :bind "bsearch" :hash
  954237325)
 int (value int) (before bool))

(defgmethod
 (packed-int-32array+duplicate :class 'packed-int-32array :bind "duplicate"
  :hash 3158844420)
 packed-int-32array)

(defgmethod
 (packed-int-32array+find :class 'packed-int-32array :bind "find" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-int-32array+rfind :class 'packed-int-32array :bind "rfind" :hash
  2984303840)
 int (value int) (from int))

(defgmethod
 (packed-int-32array+count :class 'packed-int-32array :bind "count" :hash
  4103005248)
 int (value int))

(defgmethod
 (packed-int-32array+erase :class 'packed-int-32array :bind "erase" :hash
  694024632)
 bool (value int))