(common-lisp:in-package :%godot)


(defgmethod
 (json+stringify :class 'json :bind "stringify" :hash 462733549 :static
  common-lisp:t)
 string (data variant) (indent string) (sort-keys bool) (full-precision bool))

(defgmethod
 (json+parse-string :class 'json :bind "parse_string" :hash 309047738 :static
  common-lisp:t)
 variant (json-string string))

(defgmethod (json+parse :class 'json :bind "parse" :hash 885841341) error
 (json-text string) (keep-text bool))

(defgmethod (json+get-data :class 'json :bind "get_data" :hash 1214101251)
 variant)

(defgmethod (json+set-data :class 'json :bind "set_data" :hash 1114965689)
 :void (data variant))

(defgmethod
 (json+get-parsed-text :class 'json :bind "get_parsed_text" :hash 201670096)
 string)

(defgmethod
 (json+get-error-line :class 'json :bind "get_error_line" :hash 3905245786) int)

(defgmethod
 (json+get-error-message :class 'json :bind "get_error_message" :hash
  201670096)
 string)

(defgmethod
 (json+from-native :class 'json :bind "from_native" :hash 2963479484 :static
  common-lisp:t)
 variant (variant variant) (full-objects bool))

(defgmethod
 (json+to-native :class 'json :bind "to_native" :hash 2963479484 :static
  common-lisp:t)
 variant (json variant) (allow-objects bool))