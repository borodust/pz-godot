(common-lisp:in-package :%godot)


(defgmethod
 (packed-color-array+get :class 'packed-color-array :bind "get" :hash
  2972831132)
 color (index int))

(defgmethod
 (packed-color-array+set :class 'packed-color-array :bind "set" :hash
  1444096570)
 :void (index int) (value color))

(defgmethod
 (packed-color-array+size :class 'packed-color-array :bind "size" :hash
  3173160232)
 int)

(defgmethod
 (packed-color-array+is-empty :class 'packed-color-array :bind "is_empty" :hash
  3918633141)
 bool)

(defgmethod
 (packed-color-array+push-back :class 'packed-color-array :bind "push_back"
  :hash 1007858200)
 bool (value color))

(defgmethod
 (packed-color-array+append :class 'packed-color-array :bind "append" :hash
  1007858200)
 bool (value color))

(defgmethod
 (packed-color-array+append-array :class 'packed-color-array :bind
  "append_array" :hash 798822497)
 :void (array packed-color-array))

(defgmethod
 (packed-color-array+remove-at :class 'packed-color-array :bind "remove_at"
  :hash 2823966027)
 :void (index int))

(defgmethod
 (packed-color-array+insert :class 'packed-color-array :bind "insert" :hash
  785289703)
 int (at-index int) (value color))

(defgmethod
 (packed-color-array+fill :class 'packed-color-array :bind "fill" :hash
  3730314301)
 :void (value color))

(defgmethod
 (packed-color-array+resize :class 'packed-color-array :bind "resize" :hash
  848867239)
 int (new-size int))

(defgmethod
 (packed-color-array+clear :class 'packed-color-array :bind "clear" :hash
  3218959716)
 :void)

(defgmethod
 (packed-color-array+has :class 'packed-color-array :bind "has" :hash
  3167426256)
 bool (value color))

(defgmethod
 (packed-color-array+reverse :class 'packed-color-array :bind "reverse" :hash
  3218959716)
 :void)

(defgmethod
 (packed-color-array+slice :class 'packed-color-array :bind "slice" :hash
  2451797139)
 packed-color-array (begin int) (end int))

(defgmethod
 (packed-color-array+to-byte-array :class 'packed-color-array :bind
  "to_byte_array" :hash 247621236)
 packed-byte-array)

(defgmethod
 (packed-color-array+sort :class 'packed-color-array :bind "sort" :hash
  3218959716)
 :void)

(defgmethod
 (packed-color-array+bsearch :class 'packed-color-array :bind "bsearch" :hash
  2639732838)
 int (value color) (before bool))

(defgmethod
 (packed-color-array+duplicate :class 'packed-color-array :bind "duplicate"
  :hash 3072026941)
 packed-color-array)

(defgmethod
 (packed-color-array+find :class 'packed-color-array :bind "find" :hash
  3156095363)
 int (value color) (from int))

(defgmethod
 (packed-color-array+rfind :class 'packed-color-array :bind "rfind" :hash
  3156095363)
 int (value color) (from int))

(defgmethod
 (packed-color-array+count :class 'packed-color-array :bind "count" :hash
  1682108616)
 int (value color))

(defgmethod
 (packed-color-array+erase :class 'packed-color-array :bind "erase" :hash
  1007858200)
 bool (value color))