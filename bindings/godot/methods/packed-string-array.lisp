(common-lisp:in-package :%godot)


(defgmethod
 (packed-string-array+get :class 'packed-string-array :bind "get" :hash
  2162347432)
 string (index int))

(defgmethod
 (packed-string-array+set :class 'packed-string-array :bind "set" :hash
  725585539)
 :void (index int) (value string))

(defgmethod
 (packed-string-array+size :class 'packed-string-array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-string-array+is-empty :class 'packed-string-array :bind "is_empty"
  :hash 3918633141)
 bool)

(defgmethod
 (packed-string-array+push-back :class 'packed-string-array :bind "push_back"
  :hash 816187996)
 bool (value string))

(defgmethod
 (packed-string-array+append :class 'packed-string-array :bind "append" :hash
  816187996)
 bool (value string))

(defgmethod
 (packed-string-array+append-array :class 'packed-string-array :bind
  "append_array" :hash 1120103966)
 :void (array packed-string-array))

(defgmethod
 (packed-string-array+remove-at :class 'packed-string-array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-string-array+insert :class 'packed-string-array :bind "insert" :hash
  2432393153)
 int (at-index int) (value string))

(defgmethod
 (packed-string-array+fill :class 'packed-string-array :bind "fill" :hash
  3174917410)
 :void (value string))

(defgmethod
 (packed-string-array+resize :class 'packed-string-array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-string-array+clear :class 'packed-string-array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-string-array+has :class 'packed-string-array :bind "has" :hash
  2566493496)
 bool (value string))

(defgmethod
 (packed-string-array+reverse :class 'packed-string-array :bind "reverse" :hash
  3218959716)
 :void)

(defgmethod
 (packed-string-array+slice :class 'packed-string-array :bind "slice" :hash
  2094601407)
 packed-string-array (begin int) (end int))

(defgmethod
 (packed-string-array+to-byte-array :class 'packed-string-array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-string-array+sort :class 'packed-string-array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-string-array+bsearch :class 'packed-string-array :bind "bsearch" :hash
  1171495151)
 int (value string) (before bool))

(defgmethod
 (packed-string-array+duplicate :class 'packed-string-array :bind "duplicate"
  :hash 747180633)
 packed-string-array)

(defgmethod
 (packed-string-array+find :class 'packed-string-array :bind "find" :hash
  1760645412)
 int (value string) (from int))

(defgmethod
 (packed-string-array+rfind :class 'packed-string-array :bind "rfind" :hash
  1760645412)
 int (value string) (from int))

(defgmethod
 (packed-string-array+count :class 'packed-string-array :bind "count" :hash
  2920860731)
 int (value string))

(defgmethod
 (packed-string-array+erase :class 'packed-string-array :bind "erase" :hash
  816187996)
 bool (value string))