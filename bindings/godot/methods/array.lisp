(common-lisp:in-package :%godot)


(defgmethod (array+size :class 'array :bind "size" :hash 3173160232) int)

(defgmethod (array+is-empty :class 'array :bind "is_empty" :hash 3918633141)
 bool)

(defgmethod (array+clear :class 'array :bind "clear" :hash 3218959716) :void)

(defgmethod (array+hash :class 'array :bind "hash" :hash 3173160232) int)

(defgmethod (array+assign :class 'array :bind "assign" :hash 2307260970) :void
 (array array))

(defgmethod (array+get :class 'array :bind "get" :hash 708700221) variant
 (index int))

(defgmethod (array+set :class 'array :bind "set" :hash 3798478031) :void
 (index int) (value variant))

(defgmethod (array+push-back :class 'array :bind "push_back" :hash 3316032543)
 :void (value variant))

(defgmethod
 (array+push-front :class 'array :bind "push_front" :hash 3316032543) :void
 (value variant))

(defgmethod (array+append :class 'array :bind "append" :hash 3316032543) :void
 (value variant))

(defgmethod
 (array+append-array :class 'array :bind "append_array" :hash 2307260970) :void
 (array array))

(defgmethod (array+resize :class 'array :bind "resize" :hash 848867239) int
 (size int))

(defgmethod (array+insert :class 'array :bind "insert" :hash 3176316662) int
 (position int) (value variant))

(defgmethod (array+remove-at :class 'array :bind "remove_at" :hash 2823966027)
 :void (position int))

(defgmethod (array+fill :class 'array :bind "fill" :hash 3316032543) :void
 (value variant))

(defgmethod (array+erase :class 'array :bind "erase" :hash 3316032543) :void
 (value variant))

(defgmethod (array+front :class 'array :bind "front" :hash 1460142086) variant)

(defgmethod (array+back :class 'array :bind "back" :hash 1460142086) variant)

(defgmethod
 (array+pick-random :class 'array :bind "pick_random" :hash 1460142086) variant)

(defgmethod (array+find :class 'array :bind "find" :hash 2336346817) int
 (what variant) (from int))

(defgmethod
 (array+find-custom :class 'array :bind "find_custom" :hash 2145562546) int
 (method callable) (from int))

(defgmethod (array+rfind :class 'array :bind "rfind" :hash 2336346817) int
 (what variant) (from int))

(defgmethod
 (array+rfind-custom :class 'array :bind "rfind_custom" :hash 2145562546) int
 (method callable) (from int))

(defgmethod (array+count :class 'array :bind "count" :hash 1481661226) int
 (value variant))

(defgmethod (array+has :class 'array :bind "has" :hash 3680194679) bool
 (value variant))

(defgmethod (array+pop-back :class 'array :bind "pop_back" :hash 1321915136)
 variant)

(defgmethod (array+pop-front :class 'array :bind "pop_front" :hash 1321915136)
 variant)

(defgmethod (array+pop-at :class 'array :bind "pop_at" :hash 3518259424)
 variant (position int))

(defgmethod (array+sort :class 'array :bind "sort" :hash 3218959716) :void)

(defgmethod
 (array+sort-custom :class 'array :bind "sort_custom" :hash 3470848906) :void
 (func callable))

(defgmethod (array+shuffle :class 'array :bind "shuffle" :hash 3218959716)
 :void)

(defgmethod (array+bsearch :class 'array :bind "bsearch" :hash 3372222236) int
 (value variant) (before bool))

(defgmethod
 (array+bsearch-custom :class 'array :bind "bsearch_custom" :hash 161317131)
 int (value variant) (func callable) (before bool))

(defgmethod (array+reverse :class 'array :bind "reverse" :hash 3218959716)
 :void)

(defgmethod (array+duplicate :class 'array :bind "duplicate" :hash 636440122)
 array (deep bool))

(defgmethod
 (array+duplicate-deep :class 'array :bind "duplicate_deep" :hash 1949240801)
 array (deep-subresources-mode int))

(defgmethod (array+slice :class 'array :bind "slice" :hash 1393718243) array
 (begin int) (end int) (step int) (deep bool))

(defgmethod (array+filter :class 'array :bind "filter" :hash 4075186556) array
 (method callable))

(defgmethod (array+map :class 'array :bind "map" :hash 4075186556) array
 (method callable))

(defgmethod (array+reduce :class 'array :bind "reduce" :hash 4272450342)
 variant (method callable) (accum variant))

(defgmethod (array+any :class 'array :bind "any" :hash 4129521963) bool
 (method callable))

(defgmethod (array+all :class 'array :bind "all" :hash 4129521963) bool
 (method callable))

(defgmethod (array+max :class 'array :bind "max" :hash 1460142086) variant)

(defgmethod (array+min :class 'array :bind "min" :hash 1460142086) variant)

(defgmethod (array+is-typed :class 'array :bind "is_typed" :hash 3918633141)
 bool)

(defgmethod
 (array+is-same-typed :class 'array :bind "is_same_typed" :hash 2988181878)
 bool (array array))

(defgmethod
 (array+get-typed-builtin :class 'array :bind "get_typed_builtin" :hash
  3173160232)
 int)

(defgmethod
 (array+get-typed-class-name :class 'array :bind "get_typed_class_name" :hash
  1825232092)
 string-name)

(defgmethod
 (array+get-typed-script :class 'array :bind "get_typed_script" :hash
  1460142086)
 variant)

(defgmethod
 (array+make-read-only :class 'array :bind "make_read_only" :hash 3218959716)
 :void)

(defgmethod
 (array+is-read-only :class 'array :bind "is_read_only" :hash 3918633141) bool)