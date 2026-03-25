(common-lisp:in-package :%godot)


(defgmethod
 (translation+-get-plural-message :class 'translation :bind
  "_get_plural_message" :hash 1970324172 :virtual common-lisp:t)
 string-name (src-message string-name) (src-plural-message string-name) (n int)
 (context string-name))

(defgmethod
 (translation+-get-message :class 'translation :bind "_get_message" :hash
  3639719779 :virtual common-lisp:t)
 string-name (src-message string-name) (context string-name))

(defgmethod
 (translation+set-locale :class 'translation :bind "set_locale" :hash 83702148)
 :void (locale string))

(defgmethod
 (translation+get-locale :class 'translation :bind "get_locale" :hash
  201670096)
 string)

(defgmethod
 (translation+add-message :class 'translation :bind "add_message" :hash
  3898530326)
 :void (src-message string-name) (xlated-message string-name)
 (context string-name))

(defgmethod
 (translation+add-plural-message :class 'translation :bind "add_plural_message"
  :hash 2356982266)
 :void (src-message string-name) (xlated-messages packed-string-array)
 (context string-name))

(defgmethod
 (translation+get-message :class 'translation :bind "get_message" :hash
  1829228469)
 string-name (src-message string-name) (context string-name))

(defgmethod
 (translation+get-plural-message :class 'translation :bind "get_plural_message"
  :hash 229954002)
 string-name (src-message string-name) (src-plural-message string-name) (n int)
 (context string-name))

(defgmethod
 (translation+erase-message :class 'translation :bind "erase_message" :hash
  3959009644)
 :void (src-message string-name) (context string-name))

(defgmethod
 (translation+get-message-list :class 'translation :bind "get_message_list"
  :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation+get-translated-message-list :class 'translation :bind
  "get_translated_message_list" :hash 1139954409)
 packed-string-array)

(defgmethod
 (translation+get-message-count :class 'translation :bind "get_message_count"
  :hash 3905245786)
 int)

(defgmethod
 (translation+set-plural-rules-override :class 'translation :bind
  "set_plural_rules_override" :hash 83702148)
 :void (rules string))

(defgmethod
 (translation+get-plural-rules-override :class 'translation :bind
  "get_plural_rules_override" :hash 201670096)
 string)