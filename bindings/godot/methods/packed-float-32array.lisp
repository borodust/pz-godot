(common-lisp:in-package :%godot)


(defgmethod
 (packed-float-32array+get :class 'packed-float-32array :bind "get" :hash
  1401583798)
 float (index int))

(defgmethod
 (packed-float-32array+set :class 'packed-float-32array :bind "set" :hash
  1113000516)
 :void (index int) (value float))

(defgmethod
 (packed-float-32array+size :class 'packed-float-32array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-float-32array+is-empty :class 'packed-float-32array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-float-32array+push-back :class 'packed-float-32array :bind "push_back"
  :hash 4094791666)
 bool (value float))

(defgmethod
 (packed-float-32array+append :class 'packed-float-32array :bind "append" :hash
  4094791666)
 bool (value float))

(defgmethod
 (packed-float-32array+append-array :class 'packed-float-32array :bind
  "append_array" :hash 2981316639)
 :void (array packed-float-32array))

(defgmethod
 (packed-float-32array+remove-at :class 'packed-float-32array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-float-32array+insert :class 'packed-float-32array :bind "insert" :hash
  1379903876)
 int (at-index int) (value float))

(defgmethod
 (packed-float-32array+fill :class 'packed-float-32array :bind "fill" :hash
  833936903)
 :void (value float))

(defgmethod
 (packed-float-32array+resize :class 'packed-float-32array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-float-32array+clear :class 'packed-float-32array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-float-32array+has :class 'packed-float-32array :bind "has" :hash
  1296369134)
 bool (value float))

(defgmethod
 (packed-float-32array+reverse :class 'packed-float-32array :bind "reverse"
  :hash 3218959716)
 :void)

(defgmethod
 (packed-float-32array+slice :class 'packed-float-32array :bind "slice" :hash
  1418229160)
 packed-float-32array (begin int) (end int))

(defgmethod
 (packed-float-32array+to-byte-array :class 'packed-float-32array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-float-32array+sort :class 'packed-float-32array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-float-32array+bsearch :class 'packed-float-32array :bind "bsearch"
  :hash 1175118842)
 int (value float) (before bool))

(defgmethod
 (packed-float-32array+duplicate :class 'packed-float-32array :bind "duplicate"
  :hash 3575107827)
 packed-float-32array)

(defgmethod
 (packed-float-32array+find :class 'packed-float-32array :bind "find" :hash
  1343150241)
 int (value float) (from int))

(defgmethod
 (packed-float-32array+rfind :class 'packed-float-32array :bind "rfind" :hash
  1343150241)
 int (value float) (from int))

(defgmethod
 (packed-float-32array+count :class 'packed-float-32array :bind "count" :hash
  2859915090)
 int (value float))

(defgmethod
 (packed-float-32array+erase :class 'packed-float-32array :bind "erase" :hash
  4094791666)
 bool (value float))