(common-lisp:in-package :%godot)


(defgmethod
 (packed-float-64array+get :class 'packed-float-64array :bind "get" :hash
  1401583798)
 float (index int))

(defgmethod
 (packed-float-64array+set :class 'packed-float-64array :bind "set" :hash
  1113000516)
 :void (index int) (value float))

(defgmethod
 (packed-float-64array+size :class 'packed-float-64array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-float-64array+is-empty :class 'packed-float-64array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-float-64array+push-back :class 'packed-float-64array :bind "push_back"
  :hash 4094791666)
 bool (value float))

(defgmethod
 (packed-float-64array+append :class 'packed-float-64array :bind "append" :hash
  4094791666)
 bool (value float))

(defgmethod
 (packed-float-64array+append-array :class 'packed-float-64array :bind
  "append_array" :hash 792078629)
 :void (array packed-float-64array))

(defgmethod
 (packed-float-64array+remove-at :class 'packed-float-64array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-float-64array+insert :class 'packed-float-64array :bind "insert" :hash
  1379903876)
 int (at-index int) (value float))

(defgmethod
 (packed-float-64array+fill :class 'packed-float-64array :bind "fill" :hash
  833936903)
 :void (value float))

(defgmethod
 (packed-float-64array+resize :class 'packed-float-64array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-float-64array+clear :class 'packed-float-64array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-float-64array+has :class 'packed-float-64array :bind "has" :hash
  1296369134)
 bool (value float))

(defgmethod
 (packed-float-64array+reverse :class 'packed-float-64array :bind "reverse"
  :hash 3218959716)
 :void)

(defgmethod
 (packed-float-64array+slice :class 'packed-float-64array :bind "slice" :hash
  2192974324)
 packed-float-64array (begin int) (end int))

(defgmethod
 (packed-float-64array+to-byte-array :class 'packed-float-64array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-float-64array+sort :class 'packed-float-64array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-float-64array+bsearch :class 'packed-float-64array :bind "bsearch"
  :hash 1175118842)
 int (value float) (before bool))

(defgmethod
 (packed-float-64array+duplicate :class 'packed-float-64array :bind "duplicate"
  :hash 1627308337)
 packed-float-64array)

(defgmethod
 (packed-float-64array+find :class 'packed-float-64array :bind "find" :hash
  1343150241)
 int (value float) (from int))

(defgmethod
 (packed-float-64array+rfind :class 'packed-float-64array :bind "rfind" :hash
  1343150241)
 int (value float) (from int))

(defgmethod
 (packed-float-64array+count :class 'packed-float-64array :bind "count" :hash
  2859915090)
 int (value float))

(defgmethod
 (packed-float-64array+erase :class 'packed-float-64array :bind "erase" :hash
  4094791666)
 bool (value float))