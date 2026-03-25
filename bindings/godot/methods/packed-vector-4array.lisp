(common-lisp:in-package :%godot)


(defgmethod
 (packed-vector-4array+get :class 'packed-vector-4array :bind "get" :hash
  1227817084)
 vector-4 (index int))

(defgmethod
 (packed-vector-4array+set :class 'packed-vector-4array :bind "set" :hash
  1350366223)
 :void (index int) (value vector-4))

(defgmethod
 (packed-vector-4array+size :class 'packed-vector-4array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-vector-4array+is-empty :class 'packed-vector-4array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-vector-4array+push-back :class 'packed-vector-4array :bind "push_back"
  :hash 3289167688)
 bool (value vector-4))

(defgmethod
 (packed-vector-4array+append :class 'packed-vector-4array :bind "append" :hash
  3289167688)
 bool (value vector-4))

(defgmethod
 (packed-vector-4array+append-array :class 'packed-vector-4array :bind
  "append_array" :hash 537428395)
 :void (array packed-vector-4array))

(defgmethod
 (packed-vector-4array+remove-at :class 'packed-vector-4array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-vector-4array+insert :class 'packed-vector-4array :bind "insert" :hash
  11085009)
 int (at-index int) (value vector-4))

(defgmethod
 (packed-vector-4array+fill :class 'packed-vector-4array :bind "fill" :hash
  3761353134)
 :void (value vector-4))

(defgmethod
 (packed-vector-4array+resize :class 'packed-vector-4array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-vector-4array+clear :class 'packed-vector-4array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-4array+has :class 'packed-vector-4array :bind "has" :hash
  88913544)
 bool (value vector-4))

(defgmethod
 (packed-vector-4array+reverse :class 'packed-vector-4array :bind "reverse"
  :hash 3218959716)
 :void)

(defgmethod
 (packed-vector-4array+slice :class 'packed-vector-4array :bind "slice" :hash
  2942803855)
 packed-vector-4array (begin int) (end int))

(defgmethod
 (packed-vector-4array+to-byte-array :class 'packed-vector-4array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-vector-4array+sort :class 'packed-vector-4array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-vector-4array+bsearch :class 'packed-vector-4array :bind "bsearch"
  :hash 1822067054)
 int (value vector-4) (before bool))

(defgmethod
 (packed-vector-4array+duplicate :class 'packed-vector-4array :bind "duplicate"
  :hash 146203628)
 packed-vector-4array)

(defgmethod
 (packed-vector-4array+find :class 'packed-vector-4array :bind "find" :hash
  3091171314)
 int (value vector-4) (from int))

(defgmethod
 (packed-vector-4array+rfind :class 'packed-vector-4array :bind "rfind" :hash
  3091171314)
 int (value vector-4) (from int))

(defgmethod
 (packed-vector-4array+count :class 'packed-vector-4array :bind "count" :hash
  3956594488)
 int (value vector-4))

(defgmethod
 (packed-vector-4array+erase :class 'packed-vector-4array :bind "erase" :hash
  3289167688)
 bool (value vector-4))